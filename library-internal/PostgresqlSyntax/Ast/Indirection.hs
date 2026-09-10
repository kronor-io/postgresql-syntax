module PostgresqlSyntax.Ast.Indirection where

import Control.Applicative.Combinators.NonEmpty (some)
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.IndirectionEl
import qualified PostgresqlSyntax.Ast.IndirectionEl as IndirectionEl
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import PostgresqlSyntax.Prelude hiding (some)
import PostgresqlSyntax.Settings (Settings)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- indirection:
--   |  indirection_el
--   |  indirection indirection_el
-- @
newtype Indirection = Indirection (NonEmpty IndirectionEl)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Indirection where
  toTextBuilder settings (Indirection a) = foldMap (toTextBuilder settings) a
  parser settings = Indirection <$> some (parser settings)

-- |
-- Like 'parser', but built out of 'IndirectionEl.hsParser', so the Kronor
-- @.$name@ Haskell field selector is accepted (when the option is on).
-- Only used from the @$n@ parameter position - see
-- 'PostgresqlSyntax.Ast.CExpr'.
hsParser :: Settings -> Parser Indirection
hsParser settings = Indirection <$> some (IndirectionEl.hsParser settings)

instance Qc.Arbitrary Indirection where
  shrink = Qc.genericShrink
  arbitrary = Indirection <$> Gens.nonEmptyUpTo 4 Qc.arbitrary
