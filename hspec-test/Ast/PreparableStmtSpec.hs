module Ast.PreparableStmtSpec (spec) where

import qualified Data.Text as Text
import Helpers.Specs
import PostgresqlSyntax.Ast.PreparableStmt
import PostgresqlSyntax.Settings (haskellParamFields, nullabilityMarkers)
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
  describe "Haskell param fields" $ do
    itParsesWith @PreparableStmt hsParams "select $1.$field::int8"
    itParsesWith @PreparableStmt hsParams "select $1.$a::int8, $1.$b::text"
    itParsesWith @PreparableStmt hsParams "select $1.$a::int8, $1.$a::int8"
    itParsesWith @PreparableStmt hsParams "select $2.$a::int8, $1::text"
    itParsesWith @PreparableStmt hsParams "insert into a (b, c) values ($1.$b::int8, $1.$c::text)"
    itParsesWith @PreparableStmt hsParams "select * from a where id = $1.$id::uuid"
    -- Combined with the nullability markers, which are lexically independent:
    -- @?@ suffixes a typename, @$@ prefixes a field name.
    itParsesWith @PreparableStmt (nullabilityMarkers True <> hsParams) "select $1.$a::int8?"
    itParsesWith @PreparableStmt (nullabilityMarkers True <> hsParams) "select $1.$a::text?, $1.$b::int8[]?"
    -- Off by default: standard Postgres parsing is unchanged.
    itRejectsWith @PreparableStmt mempty "select $1.$field::int8"
    itRejectsWith @PreparableStmt mempty "select $1.$a::int8, $1.$b::text"
    -- The extension is confined to the @$n@ position: a Haskell field after
    -- an ordinary column reference stays a parse error even with it on.
    itRejectsWith @PreparableStmt hsParams "select col.$field"
    itRejectsWith @PreparableStmt hsParams "select col.$field::int8"
  where
    hsParams = haskellParamFields True
