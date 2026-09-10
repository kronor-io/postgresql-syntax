-- |
-- __STUB - replaced by branch @kronor-0.5-f1@ at merge.__
--
-- Only the types are defined here, so that
-- "PostgresqlSyntax.Lowering" can be written and tested independently of the
-- concurrent work on the Haskell-target parsers, renderers and generators.
-- The real module carries the @IsAst@ and @Qc.Arbitrary@ instances too; take
-- that version wholesale when the branches meet, keeping these declarations
-- only as a cross-check that the shapes agree.
module PostgresqlSyntax.Ast.HsTargetEl
  ( HsTargetEl (..),
    HsFieldTargetEl (..),
  )
where

import PostgresqlSyntax.Ast.TargetEl
import PostgresqlSyntax.Prelude

-- |
-- Kronor extension: one element of a @SELECT@ list that may apply a Haskell
-- function, constructor or record to the SQL columns nested inside it.
data HsTargetEl
  = -- | Record construction or update: @$Name {field = ...}@.
    HsRecTargetEl Text (NonEmpty HsFieldTargetEl)
  | -- | Function or constructor application: @$name (...)@.
    HsFuncTargetEl Text [HsTargetEl]
  | -- | An ordinary SQL target - the only leaf that yields a column.
    SqlTargetEl TargetEl
  deriving (Show, Generic, Eq, Ord, Data)

-- | One @field = target@ pair of an 'HsRecTargetEl'.
data HsFieldTargetEl = HsFieldEl Text HsTargetEl
  deriving (Show, Generic, Eq, Ord, Data)
