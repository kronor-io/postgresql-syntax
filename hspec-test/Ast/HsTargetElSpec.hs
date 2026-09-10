module Ast.HsTargetElSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.HsTargetEl
import PostgresqlSyntax.Settings (haskellTargets, nullabilityMarkers)
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @HsTargetEl
  itSatisfiesArbitrary @HsTargetEl
  describe "HsFieldTargetEl" $ do
    itSatisfiesIsAst @HsFieldTargetEl
    itSatisfiesArbitrary @HsFieldTargetEl
  describe "Parsers" $ do
    itParses @HsTargetEl "a"
    itParses @HsTargetEl "a.b AS c"
    itParses @HsTargetEl "*"
  describe "extended" $ do
    describe "function targets" $ do
      itParsesWith @HsTargetEl hs "$f (a, b)"
      itParsesWith @HsTargetEl hs "$f (a)"
      itParsesWith @HsTargetEl hs "$f ()"
      itParsesWith @HsTargetEl hs "$f ($g (a), b)"
      itParsesWith @HsTargetEl hs "$Mk (a, b)"
      itParsesWith @HsTargetEl hs "$Mod.f (a)"
      itParsesWith @HsTargetEl hs "$Mod.Mk (a)"
      itRejectsWith @HsTargetEl mempty "$f (a, b)"
      itRejectsWith @HsTargetEl mempty "$f ()"
      itRejectsWith @HsTargetEl mempty "$Mod.f (a)"
    describe "record targets" $ do
      itParsesWith @HsTargetEl hs "$R {a = x::int8}"
      itParsesWith @HsTargetEl hs "$R {a = x, b = y}"
      itParsesWith @HsTargetEl hs "$r {a = x}"
      itParsesWith @HsTargetEl hs "$R {a = $f (x, y)}"
      itParsesWith @HsTargetEl hs "$R {a = $S {b = x}}"
      itRejectsWith @HsTargetEl mempty "$R {a = x::int8}"
      itRejectsWith @HsTargetEl mempty "$r {a = x}"
    describe "with nullability markers" $ do
      itParsesWith @HsTargetEl (hs <> nullabilityMarkers True) "$mk (a::int8, b::text?)"
      itParsesWith @HsTargetEl (hs <> nullabilityMarkers True) "$R {a = b::text?}"
      itRejectsWith @HsTargetEl hs "$mk (a::int8, b::text?)"
  where
    hs = haskellTargets True
