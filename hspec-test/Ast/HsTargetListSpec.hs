module Ast.HsTargetListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.HsTargetList
import PostgresqlSyntax.Settings (haskellTargets)
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @HsTargetList
  itSatisfiesArbitrary @HsTargetList
  describe "extended" $ do
    itParsesWith @HsTargetList hs "$f (a), b, $R {c = d}"
    itParsesWith @HsTargetList hs "a, $f (b, c)"
    itRejectsWith @HsTargetList mempty "$f (a), b"
  where
    hs = haskellTargets True
