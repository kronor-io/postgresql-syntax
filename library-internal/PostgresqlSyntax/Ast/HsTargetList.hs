-- |
-- Kronor extension: the 'PostgresqlSyntax.Ast.HsTargetEl.HsTargetEl'
-- counterpart of 'PostgresqlSyntax.Ast.TargetList.TargetList'.
module PostgresqlSyntax.Ast.HsTargetList where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.HsTargetEl
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- hs_target_list:
--   | hs_target_el
--   | hs_target_list ',' hs_target_el
-- @
newtype HsTargetList = HsTargetList (NonEmpty HsTargetEl)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst HsTargetList where
  toTextBuilder settings (HsTargetList a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = HsTargetList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary HsTargetList where
  shrink = Qc.genericShrink
  arbitrary = HsTargetList <$> Gens.nonEmptyUpTo 7 Qc.arbitrary
