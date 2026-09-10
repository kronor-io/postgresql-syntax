module Ast.PreparableStmtSpec (spec) where

import qualified Data.Text as Text
import Helpers.Specs
import PostgresqlSyntax.Algebra (parse, toText)
import PostgresqlSyntax.Ast.PreparableStmt
import PostgresqlSyntax.Settings (haskellTargets, nullabilityMarkers)
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @PreparableStmt
  itSatisfiesArbitrary @PreparableStmt
  describe "Parsers" $ do
    itParses @PreparableStmt
      "select i :: int8 from auth.user as u\n\
      \inner join edgenode.usere_provider as p\n\
      \on u.id = p.user_id\n\
      \inner join edgenode.provider_branch as b\n\
      \on b.provider_fk = p.provider_id"
    itParses @PreparableStmt "select * from items for update limit 1"
    itParses @PreparableStmt "select * from items limit 1 for update"
    itParses @PreparableStmt "select * from items for share limit 10"
    itParses @PreparableStmt "select * from items for no key update limit 1"
    itParses @PreparableStmt "select * from items for key share limit 1"
    itParses @PreparableStmt "select * from items for update of items nowait limit 1"
    itParses @PreparableStmt "select * from items for update skip locked limit 1"
    itParses @PreparableStmt "select * from items order by id for update limit 1"
    itParses @PreparableStmt "select * from items for update offset 5 limit 10"
  describe "Haskell targets" $ do
    itParsesWith @PreparableStmt hs "select $mkUser (id::int8, name::text) from users"
    itParsesWith @PreparableStmt hs "select $User {uid = id::int8, uname = name::text} from users"
    itParsesWith @PreparableStmt hs "select id::int8, $f (name::text) from users"
    itParsesWith @PreparableStmt hs "select $f () from users"
    itParsesWith @PreparableStmt (hs <> nullabilityMarkers True) "select $mk (a::int8, b::text?) from t"
    itRejectsWith @PreparableStmt mempty "select $mkUser (id::int8, name::text) from users"
    itRejectsWith @PreparableStmt mempty "select $User {uid = id::int8} from users"
    for_ roundtripCases $ \sql ->
      it ("Renders back faithfully: " <> Text.unpack sql) $
        (toText mempty <$> parse @PreparableStmt hs sql) `shouldBe` Right sql
  describe "Nesting depth" $ do
    itParsesWithin @PreparableStmt 5 ("select " <> Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
  describe "Error reporting" $ do
    itReportsError @PreparableStmt
      "select i :: int8 fom auth.user as u\n\
      \inner join edgenode.usere_provider as p\n\
      \on u.id = p.user_id\n\
      \inner join edgenode.provider_branch as b\n\
      \on b.provider_fk = p.provider_id"
      "(21,\"offset=21:\\nunexpected 'a'\\nexpecting end of input or white space\\n\")"
    itReportsError @PreparableStmt
      "select i :: int8 from auth.user as u\n\
      \WHERE u.id IS NO NULL && TRUE"
      "(51,\"offset=51:\\nexpecting white space\\n\")"
  describe "SourcePos error reporting" $ do
    itReportsSourcePosError @PreparableStmt
      "select i :: int8 fom auth.user as u\n\
      \inner join edgenode.usere_provider as p\n\
      \on u.id = p.user_id\n\
      \inner join edgenode.provider_branch as b\n\
      \on b.provider_fk = p.provider_id"
      "1:22 unexpected 'a'\nexpecting end of input or white space\n"
    itReportsSourcePosError @PreparableStmt
      "select i :: int8 from auth.user as u\n\
      \WHERE u.id IS NO NULL && TRUE"
      "2:15 expecting white space\n"
    itReportsSourcePosError @PreparableStmt
      "SLECT id FROM qsdqsd"
      "1:1 unexpected 'S'\nexpecting '(' or white space\n"
    itReportsSourcePosError @PreparableStmt
      "SELECT id FROM as"
      "1:18 Reserved keyword \"as\" used as an identifier. If that's what you intend, you have to wrap it in double quotes.\n"
  where
    hs = haskellTargets True
    -- Written in the renderer's canonical form (uppercase keywords, spaced
    -- casts) so that parse-then-render is the identity on the string.
    roundtripCases =
      [ "SELECT $mkUser (id :: int8, name :: text) FROM users",
        "SELECT $f () FROM users",
        "SELECT $Mod.Mk (a) FROM t",
        "SELECT $User {uid = id :: int8, uname = $f (name :: text)} FROM users",
        "SELECT a, $f (b), $R {c = d} FROM t"
      ]
