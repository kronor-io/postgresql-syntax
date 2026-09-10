module LoweringSpec (spec) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.CExpr
import PostgresqlSyntax.Ast.HsTargetEl
import PostgresqlSyntax.Ast.HsTargetList
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.IndirectionEl
import PostgresqlSyntax.Ast.PreparableStmt
import PostgresqlSyntax.Ast.TargetEl
import PostgresqlSyntax.Lowering
import PostgresqlSyntax.Settings (Settings, haskellParamFields)
import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as Qc

spec :: Spec
spec = do
  describe "renameParams" $ do
    describe "Slot allocation" $ do
      it "Gives two fields of the same parameter two slots" $
        slotsOf "select $1.$a::int8, $1.$b::text"
          `shouldBe` [((1, Just "a"), 1), ((1, Just "b"), 2)]
      it "Reuses the slot of a repeated field" $
        slotsOf "select $1.$a::int8, $1.$a::int8"
          `shouldBe` [((1, Just "a"), 1)]
      it "Reuses the slot of a bare parameter used twice" $
        slotsOf "select $1::int8, $1::int8"
          `shouldBe` [((1, Nothing), 1)]
      it "Keys a bare parameter separately from a field of the same parameter" $
        slotsOf "select $1.$a::int8, $1::text"
          `shouldBe` [((1, Nothing), 2), ((1, Just "a"), 1)]
      it "Numbers a later parameter after an earlier interpolated one" $
        slotsOf "select $1.$a::int8, $2::text"
          `shouldBe` [((1, Just "a"), 1), ((2, Nothing), 2)]
      it "Renumbers densely from a sparse, out-of-order original numbering" $
        slotsOf "select $7::int8, $3::text, $7::int8"
          `shouldBe` [((3, Nothing), 2), ((7, Nothing), 1)]

    describe "Traversal order" $ do
      it "Numbers an enclosing parameter before one nested in its indirection" $ do
        slotsOf "select $2[$1]" `shouldBe` [((1, Nothing), 2), ((2, Nothing), 1)]
        sqlOf "select $2[$1]" `shouldBe` "SELECT $1[$2]"
      it "Numbers the operands of an expression left to right" $
        sqlOf "select $3::int8 + $2::int8 + $1::int8"
          `shouldBe` "SELECT $1 :: int8 + $2 :: int8 + $3 :: int8"

    describe "Rendering after lowering" $ do
      it "Leaves plain positional placeholders and no Haskell field names" $
        sqlOf "select $1.$b::text, $1.$a::int8, $2::uuid"
          `shouldBe` "SELECT $1 :: text, $2 :: int8, $3 :: uuid"
      it "Keeps an ordinary indirection chain on a non-interpolated parameter" $
        sqlOf "select $1.a.b" `shouldBe` "SELECT $1.a.b"

    describe "A Haskell head discards the rest of its indirection chain" $ do
      -- Not reachable through the parser: the field-name predicate stops only
      -- at whitespace and @:@, so @$1.$foo.bar@ lexes as the single field
      -- @foo.bar@ (which is what makes qualified names work) rather than as a
      -- two-element chain. Asserted on a hand-built AST so the semantics is
      -- pinned anyway - the whole chain is Haskell-level addressing that the
      -- caller resolves, so none of it should reach the server.
      let ast =
            ParamCExpr
              1
              ( Just
                  ( Indirection
                      ( HsAttrNameIndirectionEl "foo"
                          :| [AttrNameIndirectionEl (UnquotedIdent "bar")]
                      )
                  )
              )
      it "Drops the chain" $
        fst (renameParams ast) `shouldBe` ParamCExpr 1 Nothing
      it "Keys the slot by the Haskell field" $
        Map.toList (snd (renameParams ast)) `shouldBe` [((1, Just "foo"), 1)]
      it "Lexes a dotted field name as one field instead" $
        slotsOf "select $1.$foo.bar::int8" `shouldBe` [((1, Just "foo.bar"), 1)]

    prop "Allocates in the order the placeholders appear in the rendered SQL" $
      \(stmt :: PreparableStmt) ->
        let rendered = toText mempty stmt
            allocated = fmap (fst . fst) (sortOn snd (Map.toList (snd (renameParams stmt))))
            appeared = renderedParamOrder rendered
         in Qc.counterexample
              (Text.unpack rendered <> "\nallocated: " <> show allocated <> "\nappeared: " <> show appeared)
              (allocated == appeared)

  describe "eraseHaskellTargets" $ do
    it "Flattens nested Haskell targets to their SQL leaves, depth-first" $
      eraseHaskellTargets
        ( HsTargetList
            ( HsFuncTargetEl
                "mk"
                [ SqlTargetEl (targetEl "a"),
                  HsRecTargetEl "R" (HsFieldEl "x" (SqlTargetEl (targetEl "b")) :| [])
                ]
                :| [SqlTargetEl (targetEl "c")]
            )
        )
        `shouldBe` HsTargetList (NonEmpty.fromList (fmap (SqlTargetEl . targetEl) ["a", "b", "c"]))
    it "Reaches target lists nested anywhere in the value" $
      eraseHaskellTargets [Just (HsTargetList (HsFuncTargetEl "f" [SqlTargetEl (targetEl "a")] :| []))]
        `shouldBe` [Just (HsTargetList (SqlTargetEl (targetEl "a") :| []))]
    it "Leaves a list with no SQL leaves alone" $
      let noLeaves = HsTargetList (HsFuncTargetEl "f" [] :| [])
       in eraseHaskellTargets noLeaves `shouldBe` noLeaves

-- * Helpers

hsParams :: Settings
hsParams = haskellParamFields True

parseStmt :: Text -> PreparableStmt
parseStmt sql = either (error . Text.unpack) id (parse @PreparableStmt hsParams sql)

-- | The 'InputParams' of a lowered statement, as a key-ordered list.
slotsOf :: Text -> [((Int, Maybe Text), Int)]
slotsOf = Map.toList . snd . lower . parseStmt

-- | A lowered statement rendered back to plain Postgres.
sqlOf :: Text -> Text
sqlOf = toText mempty . fst . lower . parseStmt

targetEl :: Text -> TargetEl
targetEl = either (error . Text.unpack) id . parse @TargetEl mempty

-- |
-- The original indices of the @$n@ placeholders of a rendered statement, in
-- order of first appearance.
--
-- A placeholder is the only thing rendering emits a bare @$@ for, but a @$@
-- can also occur inside a string constant or a quoted identifier - skipped
-- here rather than scanned - and inside an unquoted identifier, where it is a
-- legal continuation character. Hence the preceding-character check: a
-- placeholder never follows an identifier character.
renderedParamOrder :: Text -> [Int]
renderedParamOrder = nub . go ' ' . Text.unpack
  where
    go previous = \case
      [] -> []
      '\'' : rest -> go '\'' (skipQuoted '\'' rest)
      '"' : rest -> go '"' (skipQuoted '"' rest)
      '$' : rest
        | not (isIdentChar previous),
          (digits@(_ : _), rest') <- span isDigit rest ->
            read digits : go '0' rest'
      a : rest -> go a rest
    isIdentChar a = isAlphaNum a || a == '_' || a == '$'
    -- Both quotings escape the quote character by doubling it.
    skipQuoted q = \case
      [] -> []
      a : b : rest | a == q, b == q -> skipQuoted q rest
      a : rest | a == q -> rest
      _ : rest -> skipQuoted q rest
