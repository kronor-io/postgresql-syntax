-- |
-- __STUB - replaced by branch @kronor-0.5-f1@ at merge.__
-- See "PostgresqlSyntax.Ast.HsTargetEl".
module PostgresqlSyntax.Ast.HsTargetList
  ( HsTargetList (..),
  )
where

import PostgresqlSyntax.Ast.HsTargetEl
import PostgresqlSyntax.Prelude

-- |
-- Kronor extension: the @SELECT@ list of a 'PostgresqlSyntax.Ast.Targeting'
-- \'s @NormalTargeting@, mirroring 'PostgresqlSyntax.Ast.TargetList'.
newtype HsTargetList = HsTargetList (NonEmpty HsTargetEl)
  deriving (Show, Generic, Eq, Ord, Data)
