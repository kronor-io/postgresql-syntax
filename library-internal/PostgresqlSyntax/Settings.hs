-- |
-- Parse\/render options for the 'PostgresqlSyntax.Algebra' machinery.
--
-- The type is abstract: build a 'Settings' value with 'nullabilityMarkers',
-- 'haskellTargets' or 'haskellParamFields' and combine values with their
-- 'Semigroup'\/'Monoid' instances ('mempty' is faithful, standard Postgres).
-- Because no constructor or field selector is exported, a resolved
-- configuration indirection can be introduced later without a breaking change.
module PostgresqlSyntax.Settings
  ( Settings,
    nullabilityMarkers,
    haskellTargets,
    haskellParamFields,
    resolveNullabilityMarkers,
    resolveHaskellTargets,
    resolveHaskellParamFields,
  )
where

import PostgresqlSyntax.Prelude

-- |
-- Collection of parse\/render options. Use 'nullabilityMarkers',
-- 'haskellTargets' or 'haskellParamFields' to build a value, and combine
-- values with their 'Semigroup'\/'Monoid' instances ('mempty' is faithful,
-- standard Postgres).
data Settings = Settings
  { optNullabilityMarkers :: Maybe Bool,
    optHaskellTargets :: Maybe Bool,
    optHaskellParamFields :: Maybe Bool
  }
  deriving (Show, Eq)

-- | Per option the right operand wins; an unset ('Nothing') option falls back
-- to the left operand's value.
instance Semigroup Settings where
  a <> b =
    Settings
      { optNullabilityMarkers = optNullabilityMarkers b <|> optNullabilityMarkers a,
        optHaskellTargets = optHaskellTargets b <|> optHaskellTargets a,
        optHaskellParamFields = optHaskellParamFields b <|> optHaskellParamFields a
      }

instance Monoid Settings where
  mempty = Settings Nothing Nothing Nothing

-- |
-- Opt into (or out of) the 'PostgresqlSyntax.Ast.Typename' @?@ nullability
-- markers.
nullabilityMarkers :: Bool -> Settings
nullabilityMarkers x = mempty {optNullabilityMarkers = Just x}

-- |
-- Opt into (or out of) Haskell targets in a @SELECT@ list: @$f (a::int8, b::text)@
-- applies a Haskell function or constructor to the listed columns and
-- @$R {field = a::int8}@ constructs or updates a Haskell record. These are
-- Kronor extensions for the hasql-th quasiquoters; they are not Postgres syntax.
haskellTargets :: Bool -> Settings
haskellTargets x = mempty {optHaskellTargets = Just x}

-- |
-- Opt into (or out of) Haskell field selectors on parameter placeholders:
-- @$1.$field@ names a field of the first Haskell parameter. A Kronor extension
-- for the hasql-th quasiquoters; not Postgres syntax.
haskellParamFields :: Bool -> Settings
haskellParamFields x = mempty {optHaskellParamFields = Just x}

-- |
-- Resolve the nullability-marker option to its effective value, defaulting to
-- @False@ (standard Postgres). Internal - used at parse\/render sites, not
-- re-exported from the "PostgresqlSyntax" facade.
resolveNullabilityMarkers :: Settings -> Bool
resolveNullabilityMarkers = fromMaybe False . optNullabilityMarkers

-- | Resolve the Haskell-targets option, defaulting to @False@. Internal.
resolveHaskellTargets :: Settings -> Bool
resolveHaskellTargets = fromMaybe False . optHaskellTargets

-- | Resolve the Haskell-param-fields option, defaulting to @False@. Internal.
resolveHaskellParamFields :: Settings -> Bool
resolveHaskellParamFields = fromMaybe False . optHaskellParamFields
