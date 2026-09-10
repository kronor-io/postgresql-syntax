-- |
-- Lowering a parsed-with-extensions AST back to something that renders as
-- plain Postgres.
--
-- The Kronor extensions ('PostgresqlSyntax.Settings.haskellTargets' and
-- 'PostgresqlSyntax.Settings.haskellParamFields') let a statement carry
-- Haskell-level information that Postgres itself cannot see: which Haskell
-- function or record the selected columns feed, and which field of a Haskell
-- record each @$n@ placeholder takes its value from. Rendering is faithful,
-- so an AST straight out of the parser still renders that extra syntax back.
-- 'lower' is the pass that strips it: it erases the Haskell targets down to
-- their SQL leaves and renumbers the placeholders densely, handing back the
-- placeholder mapping the caller needs to build its parameter encoder.
module PostgresqlSyntax.Lowering
  ( InputParams,
    lower,
    renameParams,
    eraseHaskellTargets,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import PostgresqlSyntax.Ast.CExpr
import PostgresqlSyntax.Ast.HsTargetEl
import PostgresqlSyntax.Ast.HsTargetList
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.IndirectionEl
import PostgresqlSyntax.Ast.TargetEl
import PostgresqlSyntax.Prelude

-- |
-- Maps each distinct placeholder - keyed by its original @$n@ index and, for
-- an interpolated one, the Haskell field it selects - to the dense index it
-- was renumbered to. @Map.size@ is therefore the number of placeholders the
-- lowered statement actually has.
type InputParams = Map (Int, Maybe Text) Int

-- |
-- 'eraseHaskellTargets' followed by 'renameParams'. This is the whole
-- lowering pass; the result renders as plain Postgres.
--
-- Erasure comes first so that placeholders nested inside Haskell targets are
-- numbered in the same left-to-right order they end up in after flattening.
lower :: (Data a) => a -> (a, InputParams)
lower = renameParams . eraseHaskellTargets

-- * Placeholder renumbering

-- |
-- Renumber every @$n@ placeholder in the tree to a dense @$1..$k@, in order
-- of first appearance, and return the mapping that says how.
--
-- Two placeholders share a slot exactly when they have the same original
-- index /and/ the same Haskell field, so @$1.$a@ and @$1.$b@ get separate
-- slots while a repeated @$1.$a@, or a bare @$1@ used twice, does not.
--
-- A placeholder whose indirection /starts/ with a Haskell field selector
-- loses the rest of its indirection chain: the whole chain is Haskell-level
-- addressing that the caller resolves, not SQL the server should see. So
-- @$1.$foo[2]@ lowers to a bare @$3@ (say), keyed @(1, Just "foo")@. An
-- ordinary indirection keeps its chain untouched.
renameParams :: (Data a) => a -> (a, InputParams)
renameParams a = runAlloc (topDownM (mkM step) a) Map.empty
  where
    step :: CExpr -> Alloc CExpr
    step = \case
      ParamCExpr n Nothing ->
        (`ParamCExpr` Nothing) <$> getNextParam n Nothing
      ParamCExpr n (Just indirection@(Indirection (el :| _))) -> case el of
        HsAttrNameIndirectionEl field ->
          (`ParamCExpr` Nothing) <$> getNextParam n (Just field)
        _ ->
          (\n' -> ParamCExpr n' (Just indirection)) <$> getNextParam n Nothing
      other -> pure other

-- |
-- Look up the slot for a placeholder, allocating the next one if it is the
-- first time this @(index, field)@ pair is seen.
getNextParam :: Int -> Maybe Text -> Alloc Int
getNextParam param field =
  Alloc $ \seen ->
    let key = (param, field)
     in case Map.lookup key seen of
          Just slot -> (slot, seen)
          Nothing ->
            let slot = Map.size seen + 1
             in (slot, Map.insert key slot seen)

-- * Haskell-target erasure

-- |
-- Replace every 'HsTargetList' in the tree by the flat list of the
-- 'SqlTargetEl' leaves reachable from it, collected depth-first and
-- left-to-right - i.e. in the order the columns come back from the server.
--
-- The type is kept: what changes is that afterwards every element is a
-- 'SqlTargetEl', so rendering emits an ordinary SQL target list.
--
-- A list with no SQL leaves at all (only a nullary Haskell function target,
-- say) is left alone rather than turned into an empty target list, which the
-- 'NonEmpty' cannot represent. Such a statement selects no columns and is not
-- valid SQL either way.
eraseHaskellTargets :: (Data a) => a -> a
eraseHaskellTargets = everywhere (mkT flattenHsTargetList)

flattenHsTargetList :: HsTargetList -> HsTargetList
flattenHsTargetList original@(HsTargetList els) =
  case NonEmpty.nonEmpty (foldMap sqlTargetEls els) of
    Nothing -> original
    Just leaves -> HsTargetList (fmap SqlTargetEl leaves)

sqlTargetEls :: HsTargetEl -> [TargetEl]
sqlTargetEls = \case
  SqlTargetEl a -> [a]
  HsFuncTargetEl _ as -> foldMap sqlTargetEls as
  HsRecTargetEl _ as -> foldMap (\(HsFieldEl _ a) -> sqlTargetEls a) as

-- * Generic traversal
--
-- $generics
--
-- Hand-rolled against "Data.Data" rather than pulled in from @syb@: it is
-- four functions, and this package would otherwise gain a dependency purely
-- for them.

-- |
-- Apply a generic monadic transformation to every node, visiting a node
-- /before/ its children and the children left-to-right in field order.
--
-- Pre-order is what 'renameParams' needs: @syb@\'s bottom-up @everywhereM@
-- would number the inner placeholder of @$1[$2]@ first, and the numbering has
-- to follow source order for the caller\'s parameter tuple to line up.
topDownM :: (Monad m, Data a) => (forall d. (Data d) => d -> m d) -> a -> m a
topDownM f x = f x >>= gmapM (topDownM f)

-- |
-- Apply a generic transformation to every node, bottom-up.
everywhere :: (Data a) => (forall d. (Data d) => d -> d) -> a -> a
everywhere f = f . gmapT (everywhere f)

-- |
-- Lift a monadic transformation on one type into a generic one that is the
-- identity everywhere else.
mkM :: forall a b m. (Typeable a, Typeable b, Typeable m, Monad m) => (b -> m b) -> a -> m a
mkM f = fromMaybe pure (cast f)

-- | Lift a transformation on one type into a generic one.
mkT :: forall a b. (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f = fromMaybe id (cast f)

-- * The allocation monad
--
-- $alloc
--
-- A state monad over 'InputParams', spelled out here so that neither @mtl@
-- nor @transformers@ has to be added to this package's dependencies.

newtype Alloc a = Alloc {runAlloc :: InputParams -> (a, InputParams)}

instance Functor Alloc where
  fmap f (Alloc g) = Alloc $ \s -> case g s of
    (a, s') -> (f a, s')

instance Applicative Alloc where
  pure a = Alloc $ \s -> (a, s)
  Alloc f <*> Alloc g = Alloc $ \s -> case f s of
    (h, s') -> case g s' of
      (a, s'') -> (h a, s'')

instance Monad Alloc where
  Alloc g >>= f = Alloc $ \s -> case g s of
    (a, s') -> runAlloc (f a) s'
