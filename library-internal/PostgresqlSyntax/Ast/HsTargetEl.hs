-- |
-- Kronor extension: Haskell-valued entries in a @SELECT@ list.
--
-- Not Postgres syntax. The productions in this module are only available
-- when 'PostgresqlSyntax.Settings.haskellTargets' is on; with the option
-- off (the 'mempty' default) an 'HsTargetEl' is exactly a
-- 'PostgresqlSyntax.Ast.TargetEl.TargetEl' wrapped in 'SqlTargetEl', so
-- upstream Postgres parsing is unchanged.
module PostgresqlSyntax.Ast.HsTargetEl where

import qualified Data.Text as Text
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.TargetEl
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.Shrinks as Shrinks
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (Settings, resolveHaskellTargets)
import qualified Test.QuickCheck as Qc
import qualified TextBuilder

-- |
-- ==== References
-- @
-- hs_target_el:
--   |  '$' hs_name '{' hs_field_target_list '}'
--   |  '$' hs_name '(' hs_target_list? ')'
--   |  target_el
-- @
data HsTargetEl
  = -- | @$R {a = x, b = y}@ - a Haskell record construction (uppercase
    -- name) or record update (lowercase name).
    HsRecTargetEl Text (NonEmpty HsFieldTargetEl)
  | -- | @$f (a, b)@ - a Haskell function or constructor applied to the
    -- listed targets. The argument list may be empty: @$f ()@.
    HsFuncTargetEl Text [HsTargetEl]
  | -- | An ordinary SQL target.
    SqlTargetEl TargetEl
  deriving (Show, Generic, Eq, Ord, Data)

-- |
-- ==== References
-- @
-- hs_field_target_el:
--   |  hs_name '=' hs_target_el
-- @
data HsFieldTargetEl = HsFieldEl Text HsTargetEl
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst HsTargetEl where
  toTextBuilder settings = \case
    HsRecTargetEl a b -> "$" <> TextBuilder.text a <> " {" <> TextBuilders.commaNonEmpty (toTextBuilder settings) b <> "}"
    HsFuncTargetEl a b -> "$" <> TextBuilder.text a <> " (" <> mconcat (intersperse ", " (fmap (toTextBuilder settings) b)) <> ")"
    SqlTargetEl a -> toTextBuilder settings a

  parser settings =
    if resolveHaskellTargets settings
      then
        -- The '$'-prefixed alternatives come first: neither commits to a
        -- head before its distinguishing delimiter, so both stay
        -- backtrackable, but placing them after the SQL target would let a
        -- committed SQL branch swallow them.
        Parser.label "haskell target" $
          asum
            [ hsRecTarget settings,
              hsFuncTarget settings,
              SqlTargetEl <$> parser settings
            ]
      else SqlTargetEl <$> parser settings

-- |
-- @$R {a = x, b = y}@. The name predicate is deliberately permissive - any
-- run of characters that is neither a space nor the opening brace - so that
-- qualified names like @$Mod.R@ work.
hsRecTarget :: Settings -> Parser HsTargetEl
hsRecTarget settings =
  Parser.label "haskell record target" $ do
    Parsers.char '$'
    recConsName <- Parsers.takeWhile1P (Just "Haskell record constructor") (\x -> not (isSpace x || x == '{'))
    Parsers.space
    Parsers.char '{'
    elts <- Parsers.sep1 Parsers.commaSeparator (parser settings)
    Parsers.space
    Parsers.char '}'
    pure (HsRecTargetEl recConsName elts)

-- |
-- @$f (a, b)@, including the nullary @$f ()@. The name predicate stops at a
-- space or the opening paren, so qualified names like @$Mod.f@ work.
hsFuncTarget :: Settings -> Parser HsTargetEl
hsFuncTarget settings =
  Parser.label "haskell function target" $ do
    Parsers.char '$'
    funcName <- Parsers.takeWhile1P (Just "Haskell function") (\x -> not (isSpace x || x == '('))
    Parsers.space
    Parsers.char '('
    Parsers.space
    elts <- (toList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)) <|> pure []
    Parsers.space
    Parsers.char ')'
    pure (HsFuncTargetEl funcName elts)

instance IsAst HsFieldTargetEl where
  toTextBuilder settings (HsFieldEl a b) = TextBuilder.text a <> " = " <> toTextBuilder settings b
  parser settings =
    Parser.label "haskell field target" $ do
      Parsers.space
      fieldName <- Parsers.takeWhile1P (Just "Haskell field name") (\x -> not (isSpace x || x == '='))
      Parsers.space
      Parsers.char '='
      Parsers.space
      HsFieldEl fieldName <$> parser settings

-- |
-- Only 'SqlTargetEl' is ever generated. The extension constructors parse
-- only under 'PostgresqlSyntax.Settings.haskellTargets', while
-- 'PostgresqlSyntax.Algebra.isAstProperties' roundtrips at 'mempty' - same
-- reasoning as 'PostgresqlSyntax.Ast.Typename.Typename''s hardcoded
-- nullability flags.
instance Qc.Arbitrary HsTargetEl where
  shrink = \case
    SqlTargetEl a -> SqlTargetEl <$> Qc.shrink a
    HsRecTargetEl a b -> HsRecTargetEl a <$> Qc.shrink b
    HsFuncTargetEl a b -> HsFuncTargetEl a <$> Qc.shrink b
  arbitrary = SqlTargetEl <$> Qc.arbitrary

-- |
-- Unreachable from 'HsTargetEl''s generator (which never produces
-- 'HsRecTargetEl'); it exists so this type gets the same per-module
-- property spec as its siblings.
instance Qc.Arbitrary HsFieldTargetEl where
  shrink (HsFieldEl a b) =
    [HsFieldEl a' b | a' <- Shrinks.nonEmptyText a] <> [HsFieldEl a b' | b' <- Qc.shrink b]
  arbitrary = HsFieldEl <$> genFieldName <*> Gens.downscale Qc.arbitrary
    where
      genFieldName = Text.pack <$> Qc.listOf1 (Qc.elements ['a' .. 'z'])
