-- Rename params $1.userId, $1.userName, $2 to be $1, $2, $3

module PostgresqlSyntax.Renaming where

import Data.Bitraversable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import PostgresqlSyntax.Ast
import qualified PostgresqlSyntax.Extras.NonEmpty as NonEmpty
import PostgresqlSyntax.Extras.TextBuilder
import PostgresqlSyntax.Prelude hiding (aExpr, bit, fromList, many, option, sortBy, try)
import Text.Builder hiding (char7, doubleDec, int64Dec, intDec)
import Control.Monad.State (State, get, put)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type InputParams = Map (Int, Maybe Text) Int

-- * Statements

preparableStmt :: PreparableStmt -> State InputParams PreparableStmt
preparableStmt = \case
  SelectPreparableStmt a -> SelectPreparableStmt <$> selectStmt a
  InsertPreparableStmt a -> InsertPreparableStmt <$> insertStmt a
  UpdatePreparableStmt a -> UpdatePreparableStmt <$> updateStmt a
  DeletePreparableStmt a -> DeletePreparableStmt <$> deleteStmt a
  CallPreparableStmt a -> CallPreparableStmt <$> callStmt a

-- * Call

callStmt (CallStmt a) = CallStmt <$> funcApplication a

-- * Insert

insertStmt (InsertStmt a b c d e) =
  InsertStmt <$>
    traverse withClause a
    <*> insertTarget b
    <*> insertRest c
    <*> traverse onConflict d
    <*> traverse returningClause e

insertTarget (InsertTarget a b) =
  InsertTarget <$> qualifiedName a <*> traverse colId b

insertRest = \case
  SelectInsertRest a b c ->
    SelectInsertRest <$>
      traverse insertColumnList a
      <*> pure b
      <*> selectStmt c
  x -> pure x

insertColumnList = traverse insertColumnItem

insertColumnItem (InsertColumnItem a b) =
  InsertColumnItem <$>
    colId a
    <*> traverse indirection b

onConflict (OnConflict a b) =
  OnConflict <$>
    traverse confExpr a
    <*> onConflictDo b

onConflictDo = \case
  UpdateOnConflictDo a b ->
    UpdateOnConflictDo <$>
      setClauseList a
      <*> traverse whereClause b
  x -> pure x

confExpr = \case
  WhereConfExpr a b ->
    WhereConfExpr <$>
      indexParams a
      <*> traverse whereClause b
  x -> pure x

returningClause = targetList

-- * Update

updateStmt (UpdateStmt a b c d e f) =
  UpdateStmt <$>
    traverse withClause a
    <*> relationExprOptAlias b
    <*> setClauseList c
    <*> traverse fromClause d
    <*> traverse whereOrCurrentClause e
    <*> traverse returningClause f

setClauseList = traverse setClause

setClause = \case
  TargetSetClause a b ->
    TargetSetClause <$>
      setTarget a
      <*> aExpr b
  TargetListSetClause a b ->
    TargetListSetClause <$>
      setTargetList a
      <*> aExpr b

setTarget (SetTarget a b) =
  SetTarget <$>
    colId a
    <*> traverse indirection b

setTargetList = traverse setTarget

-- * Delete

deleteStmt (DeleteStmt a b c d e) =
  DeleteStmt <$>
    traverse withClause a
    <*> relationExprOptAlias b
    <*> traverse usingClause c
    <*> traverse whereOrCurrentClause d
    <*> traverse returningClause e

usingClause = fromList

-- * Select
selectStmt :: SelectStmt -> State InputParams SelectStmt
selectStmt = \case
  Left a -> Left <$> selectNoParens a
  Right a -> Right <$> selectWithParens a

selectNoParens (SelectNoParens a b c d e) =
  SelectNoParens <$>
    traverse withClause a
    <*> selectClause b
    <*> traverse sortClause c
    <*> traverse selectLimit d
    <*> traverse forLockingClause e

selectWithParens :: SelectWithParens -> State InputParams SelectWithParens
selectWithParens = \case
  NoParensSelectWithParens a -> NoParensSelectWithParens <$> selectNoParens a
  WithParensSelectWithParens a -> WithParensSelectWithParens <$> selectWithParens a

withClause (WithClause a b) =
  WithClause a <$> traverse commonTableExpr b

commonTableExpr (CommonTableExpr a b c d) =
  CommonTableExpr <$>
    ident a
    <*> traverse (traverse ident) b
    <*> pure c
    <*> preparableStmt d

selectLimit = \case
  LimitOffsetSelectLimit a b ->
    LimitOffsetSelectLimit <$>
      limitClause a
      <*> offsetClause b
  OffsetLimitSelectLimit a b ->
    OffsetLimitSelectLimit <$>
      offsetClause a
      <*> limitClause b
  LimitSelectLimit a -> LimitSelectLimit <$> limitClause a
  OffsetSelectLimit a -> OffsetSelectLimit <$> offsetClause a

limitClause = \case
  LimitLimitClause a b ->
    LimitLimitClause <$>
      selectLimitValue a
      <*> traverse aExpr b
  FetchOnlyLimitClause a b c ->
    FetchOnlyLimitClause <$>
      pure a
      <*> traverse selectFetchFirstValue b
      <*> pure c

selectFetchFirstValue = \case
  ExprSelectFetchFirstValue a -> ExprSelectFetchFirstValue <$> cExpr a
  x -> pure x

selectLimitValue = \case
  ExprSelectLimitValue a -> ExprSelectLimitValue <$> aExpr a
  x -> pure x

offsetClause = \case
  ExprOffsetClause a -> ExprOffsetClause <$> aExpr a
  FetchFirstOffsetClause a b -> FetchFirstOffsetClause <$> selectFetchFirstValue a <*> pure b

forLockingClause = \case
  ItemsForLockingClause a -> ItemsForLockingClause <$> traverse forLockingItem a
  x -> pure x

forLockingItem (ForLockingItem a b c) =
  ForLockingItem <$>
    forLockingStrength a
    <*> traverse (traverse qualifiedName) b
    <*> pure c

forLockingStrength = pure

selectClause = bitraverse simpleSelect selectWithParens

simpleSelect = \case
  NormalSimpleSelect a b c d e f g ->
    NormalSimpleSelect <$>
      traverse targeting a
      <*> traverse intoClause b
      <*> traverse fromClause c
      <*> traverse whereClause d
      <*> traverse groupClause e
      <*> traverse havingClause f
      <*> traverse windowClause g
  ValuesSimpleSelect a -> ValuesSimpleSelect <$> valuesClause a
  TableSimpleSelect a -> TableSimpleSelect <$> relationExpr a
  BinSimpleSelect a b c d -> BinSimpleSelect <$>
    selectBinOp a
    <*> selectClause b
    <*> pure c
    <*> selectClause d

selectBinOp = pure

targeting = \case
  NormalTargeting a -> NormalTargeting <$> traverse hsTargetEl a
  AllTargeting a -> AllTargeting <$> traverse targetList a
  DistinctTargeting a b -> DistinctTargeting <$> traverse onExpressionsClause a <*> traverse targetEl b

hsTargetEl = \case
  HsRecTargetEl a b -> HsRecTargetEl a <$> traverse hsFieldTargetEl b
  HsFuncTargetEl a b -> HsFuncTargetEl a <$> traverse hsTargetEl b
  SqlTargetEl a -> SqlTargetEl <$> targetEl a

hsFieldTargetEl (HsFieldEl a b) = HsFieldEl a <$> hsTargetEl b

targetList = traverse targetEl

onExpressionsClause a = traverse aExpr a

targetEl = \case
  AliasedExprTargetEl a b -> AliasedExprTargetEl <$> aExpr a <*> ident b
  ImplicitlyAliasedExprTargetEl a b -> ImplicitlyAliasedExprTargetEl <$> aExpr a <*> ident b
  ExprTargetEl a -> ExprTargetEl <$> aExpr a
  x -> pure x

-- * Select Into

intoClause a = optTempTableName a

optTempTableName = \case
  TemporaryOptTempTableName a b -> TemporaryOptTempTableName a <$> qualifiedName b
  TempOptTempTableName a b -> TempOptTempTableName a <$> qualifiedName b
  LocalTemporaryOptTempTableName a b -> LocalTemporaryOptTempTableName a <$> qualifiedName b
  LocalTempOptTempTableName a b -> LocalTempOptTempTableName a <$> qualifiedName b
  GlobalTemporaryOptTempTableName a b -> GlobalTemporaryOptTempTableName a <$> qualifiedName b
  GlobalTempOptTempTableName a b -> GlobalTempOptTempTableName a <$> qualifiedName b
  UnloggedOptTempTableName a b -> UnloggedOptTempTableName a <$> qualifiedName b
  TableOptTempTableName a -> TableOptTempTableName <$> qualifiedName a
  QualifedOptTempTableName a -> QualifedOptTempTableName <$> qualifiedName a

-- * From

fromClause a = fromList a

fromList = traverse tableRef

tableRef = \case
  RelationExprTableRef a b c -> RelationExprTableRef <$>
    relationExpr a
    <*> traverse aliasClause b
    <*> traverse tablesampleClause c
  FuncTableRef a b c -> FuncTableRef <$>
    pure a
    <*> funcTable b
    <*> traverse funcAliasClause c
  SelectTableRef a b c -> SelectTableRef <$>
    pure a
    <*> selectWithParens b
    <*> traverse aliasClause c
  JoinTableRef a b -> JoinTableRef <$>
    joinedTable a
    <*> traverse aliasClause b

relationExpr = \case
  SimpleRelationExpr a b -> flip SimpleRelationExpr b <$> qualifiedName a
  OnlyRelationExpr a b -> flip OnlyRelationExpr b <$> qualifiedName a

relationExprOptAlias (RelationExprOptAlias a b) = RelationExprOptAlias <$>
  relationExpr a
  <*> traverse optAlias b

optAlias (a, b) = (a,) <$> colId b

tablesampleClause (TablesampleClause a b c) =
  TablesampleClause <$>
    funcName a
    <*> exprList b
    <*> traverse repeatableClause c

repeatableClause a = aExpr a

funcTable = \case
  FuncExprFuncTable a b -> flip FuncExprFuncTable b <$> funcExprWindownless a
  RowsFromFuncTable a b -> flip RowsFromFuncTable b <$> rowsfromList a

rowsfromItem (RowsfromItem a b) = RowsfromItem <$>
  funcExprWindownless a
  <*> traverse colDefList b

rowsfromList = traverse rowsfromItem

colDefList a = tableFuncElementList a

tableFuncElementList = traverse tableFuncElement

tableFuncElement (TableFuncElement a b c) = TableFuncElement <$>
  colId a
  <*> typename b
  <*> traverse collateClause c

collateClause a = anyName a

aliasClause :: AliasClause -> State InputParams AliasClause
aliasClause (AliasClause a b c) = AliasClause a <$>
  colId b
  <*> traverse (traverse colId) c

funcAliasClause = \case
  AliasFuncAliasClause a -> AliasFuncAliasClause <$> aliasClause a
  AsFuncAliasClause a -> AsFuncAliasClause <$> tableFuncElementList a
  AsColIdFuncAliasClause a b -> AsColIdFuncAliasClause <$> colId a <*> tableFuncElementList b
  ColIdFuncAliasClause a b -> ColIdFuncAliasClause <$> colId a <*> tableFuncElementList b

joinedTable = \case
  InParensJoinedTable a -> InParensJoinedTable <$> joinedTable a
  MethJoinedTable a b c -> case a of
    CrossJoinMeth -> MethJoinedTable CrossJoinMeth <$> tableRef b <*> tableRef c
    QualJoinMeth d e -> MethJoinedTable <$>
      (QualJoinMeth <$> traverse joinType d <*> joinQual e)
      <*> tableRef b
      <*> tableRef c
    NaturalJoinMeth d -> MethJoinedTable <$>
      (NaturalJoinMeth <$> traverse joinType d)
      <*> tableRef b
      <*> tableRef c

joinType = pure

joinQual = \case
  UsingJoinQual a -> UsingJoinQual <$> traverse ident a
  OnJoinQual a -> OnJoinQual <$> aExpr a

whereClause a = aExpr a

whereOrCurrentClause = \case
  ExprWhereOrCurrentClause a -> ExprWhereOrCurrentClause <$> aExpr a
  CursorWhereOrCurrentClause a -> CursorWhereOrCurrentClause <$> cursorName a

-- * Group By

groupClause a = traverse groupByItem a

groupByItem = \case
  ExprGroupByItem a -> ExprGroupByItem <$> aExpr a
  EmptyGroupingSetGroupByItem -> pure EmptyGroupingSetGroupByItem
  RollupGroupByItem a -> RollupGroupByItem <$> traverse aExpr a
  CubeGroupByItem a -> CubeGroupByItem <$> traverse aExpr a
  GroupingSetsGroupByItem a -> GroupingSetsGroupByItem <$> traverse groupByItem a

-- * Having

havingClause a = aExpr a

-- * Window

windowClause a = traverse windowDefinition a

windowDefinition (WindowDefinition a b) = WindowDefinition <$> ident a <*> windowSpecification b

windowSpecification (WindowSpecification a b c d) =
  WindowSpecification <$>
    traverse ident a
    <*> traverse partitionClause b
    <*> traverse sortClause c
    <*> traverse frameClause d

partitionClause a = traverse aExpr a

frameClause (FrameClause a b c) =
  FrameClause a <$>
    frameExtent b
    <*> pure c

frameExtent = \case
  SingularFrameExtent a -> SingularFrameExtent <$> frameBound a
  BetweenFrameExtent a b -> BetweenFrameExtent <$> frameBound a <*> frameBound b

frameBound = \case
  PrecedingFrameBound a -> PrecedingFrameBound <$> aExpr a
  FollowingFrameBound a -> FollowingFrameBound <$> aExpr a
  x -> pure x

-- * Order By

sortClause a = traverse sortBy a

sortBy = \case
  UsingSortBy a b c -> UsingSortBy <$> aExpr a <*> qualAllOp b <*> traverse nullsOrder c
  AscDescSortBy a b c -> AscDescSortBy <$> aExpr a <*> traverse ascDesc b <*> traverse nullsOrder c

-- * Values

valuesClause a = traverse (traverse aExpr) a

-- * Exprs

exprList = traverse aExpr

aExpr = \case
  CExprAExpr a -> CExprAExpr <$> cExpr a
  TypecastAExpr a b -> TypecastAExpr <$> aExpr a <*> typename b
  CollateAExpr a b -> CollateAExpr <$> aExpr a <*> anyName b
  AtTimeZoneAExpr a b -> AtTimeZoneAExpr <$> aExpr a <*> aExpr b
  PlusAExpr a -> PlusAExpr <$> aExpr a
  MinusAExpr a -> MinusAExpr <$> aExpr a
  SymbolicBinOpAExpr a b c -> SymbolicBinOpAExpr <$> aExpr a <*> symbolicExprBinOp b <*> aExpr c
  PrefixQualOpAExpr a b -> PrefixQualOpAExpr <$> qualOp a <*> aExpr b
  SuffixQualOpAExpr a b -> SuffixQualOpAExpr <$> aExpr a <*> qualOp b
  AndAExpr a b -> AndAExpr <$> aExpr a <*> aExpr b
  OrAExpr a b -> OrAExpr <$> aExpr a <*> aExpr b
  NotAExpr a -> NotAExpr <$> aExpr a
  VerbalExprBinOpAExpr a b c d e -> VerbalExprBinOpAExpr <$> aExpr a <*> pure b <*> verbalExprBinOp c <*> aExpr d <*> traverse aExpr e
  ReversableOpAExpr a b c -> ReversableOpAExpr <$> aExpr a <*> pure b <*> aExprReversableOp c
  IsnullAExpr a -> IsnullAExpr <$> aExpr a
  NotnullAExpr a -> NotnullAExpr <$> aExpr a
  OverlapsAExpr a b -> OverlapsAExpr <$> row a <*> row b
  SubqueryAExpr a b c d -> SubqueryAExpr <$> aExpr a <*> subqueryOp b <*> subType c <*> bitraverse selectWithParens aExpr d
  UniqueAExpr a -> UniqueAExpr <$> selectWithParens a
  x -> pure x

bExpr = \case
  CExprBExpr a -> CExprBExpr <$> cExpr a
  TypecastBExpr a b -> TypecastBExpr <$> bExpr a <*> typename b
  PlusBExpr a -> PlusBExpr <$> bExpr a
  MinusBExpr a -> MinusBExpr <$> bExpr a
  SymbolicBinOpBExpr a b c -> SymbolicBinOpBExpr <$> bExpr a <*> symbolicExprBinOp b <*> bExpr c
  QualOpBExpr a b -> QualOpBExpr <$> qualOp a <*> bExpr b
  IsOpBExpr a b c -> IsOpBExpr <$> bExpr a <*> pure b <*> bExprIsOp c

cExpr = \case
  ColumnrefCExpr a -> ColumnrefCExpr <$> columnref a
  AexprConstCExpr a -> AexprConstCExpr <$> aexprConst a
  ParamCExpr a b -> do
    case b of
      Nothing -> do
        rParam <- getNextParam a Nothing
        pure $ ParamCExpr rParam Nothing
      Just indList -> do
        (rParam, indList') <- hsIndirection a indList
        pure $ ParamCExpr rParam (Just indList')
  InParensCExpr a b -> InParensCExpr <$> aExpr a <*> traverse indirection b
  CaseCExpr a -> CaseCExpr <$> caseExpr a
  FuncCExpr a -> FuncCExpr <$> funcExpr a
  SelectWithParensCExpr a b -> SelectWithParensCExpr <$> selectWithParens a <*> traverse indirection b
  ExistsCExpr a -> ExistsCExpr <$> selectWithParens a
  ArrayCExpr a -> ArrayCExpr <$> bitraverse selectWithParens arrayExpr a
  ExplicitRowCExpr a -> ExplicitRowCExpr <$> explicitRow a
  ImplicitRowCExpr a -> ImplicitRowCExpr <$> implicitRow a
  GroupingCExpr a -> GroupingCExpr <$> exprList a

-- * Ops

aExprReversableOp = \case
  DistinctFromAExprReversableOp b -> DistinctFromAExprReversableOp <$> aExpr b
  OfAExprReversableOp b -> OfAExprReversableOp <$> typeList b
  BetweenAExprReversableOp b c d -> BetweenAExprReversableOp b <$> bExpr c <*> aExpr d
  BetweenSymmetricAExprReversableOp b c -> BetweenSymmetricAExprReversableOp <$> bExpr b <*> aExpr c
  InAExprReversableOp b -> InAExprReversableOp <$> inExpr b
  x -> pure x

verbalExprBinOp = pure

subqueryOp = \case
  AllSubqueryOp a -> AllSubqueryOp <$> allOp a
  AnySubqueryOp a -> AnySubqueryOp <$> anyOperator a
  x -> pure x

bExprIsOp = \case
    DistinctFromBExprIsOp b -> DistinctFromBExprIsOp <$> bExpr b
    OfBExprIsOp a -> OfBExprIsOp <$> typeList a
    x -> pure x

symbolicExprBinOp = \case
  MathSymbolicExprBinOp a -> MathSymbolicExprBinOp <$> mathOp a
  QualSymbolicExprBinOp a -> QualSymbolicExprBinOp <$> qualOp a

qualOp = \case
  OpQualOp a -> OpQualOp <$> pure a
  OperatorQualOp a -> OperatorQualOp <$> anyOperator a

qualAllOp = \case
  AllQualAllOp a -> AllQualAllOp <$> allOp a
  AnyQualAllOp a -> AnyQualAllOp <$> anyOperator a

anyOperator = \case
  AllOpAnyOperator a -> AllOpAnyOperator <$> allOp a
  QualifiedAnyOperator a b -> QualifiedAnyOperator <$> colId a <*> anyOperator b

allOp = \case
  OpAllOp a -> OpAllOp <$> pure a
  MathAllOp a -> MathAllOp <$> mathOp a

mathOp = pure

-- *

inExpr = \case
  SelectInExpr a -> SelectInExpr <$> selectWithParens a
  ExprListInExpr a -> ExprListInExpr <$> exprList a

caseExpr (CaseExpr a b c) =
  CaseExpr <$>
    traverse aExpr a
    <*> traverse whenClause b
    <*> traverse caseDefault c

whenClause (WhenClause a b) = WhenClause <$> aExpr a <*> aExpr b

caseDefault a = aExpr a

arrayExpr :: ArrayExpr -> State InputParams ArrayExpr
arrayExpr = \case
  ExprListArrayExpr a -> ExprListArrayExpr <$> exprList a
  ArrayExprListArrayExpr a -> ArrayExprListArrayExpr <$> arrayExprList a
  x -> pure x

arrayExprList = traverse arrayExpr

row = \case
  ExplicitRowRow a -> ExplicitRowRow <$> explicitRow a
  ImplicitRowRow a -> ImplicitRowRow <$> implicitRow a

explicitRow a = traverse exprList a

implicitRow (ImplicitRow a b) = ImplicitRow <$> exprList a <*> aExpr b

funcApplication (FuncApplication a b) =
  FuncApplication <$> funcName a <*> traverse funcApplicationParams b

funcApplicationParams = \case
  NormalFuncApplicationParams a b c -> NormalFuncApplicationParams a <$>
    traverse funcArgExpr b
    <*> traverse sortClause c
  VariadicFuncApplicationParams a b c -> VariadicFuncApplicationParams <$>
    traverse (traverse funcArgExpr) a
    <*> funcArgExpr b
    <*> traverse sortClause c
  x -> pure x

allOrDistinct = pure

funcArgExpr = \case
  ExprFuncArgExpr a -> ExprFuncArgExpr <$> aExpr a
  ColonEqualsFuncArgExpr a b -> ColonEqualsFuncArgExpr <$> ident a <*> aExpr b
  EqualsGreaterFuncArgExpr a b -> EqualsGreaterFuncArgExpr <$> ident a <*> aExpr b

-- ** Func Expr

funcExpr = \case
  ApplicationFuncExpr a b c d -> ApplicationFuncExpr <$>
      funcApplication a
      <*> traverse withinGroupClause b
      <*> traverse filterClause c
      <*> traverse overClause d
  SubexprFuncExpr a -> SubexprFuncExpr <$> funcExprCommonSubexpr a

funcExprWindownless = \case
  ApplicationFuncExprWindowless a -> ApplicationFuncExprWindowless <$> funcApplication a
  CommonSubexprFuncExprWindowless a -> CommonSubexprFuncExprWindowless <$> funcExprCommonSubexpr a

withinGroupClause a = sortClause a

filterClause a =  aExpr a

overClause = \case
  WindowOverClause a -> WindowOverClause <$> windowSpecification a
  ColIdOverClause a -> ColIdOverClause <$> colId a

funcExprCommonSubexpr = \case
  CollationForFuncExprCommonSubexpr a -> CollationForFuncExprCommonSubexpr <$> aExpr a
  CastFuncExprCommonSubexpr a b -> CastFuncExprCommonSubexpr <$> aExpr a <*> typename b
  ExtractFuncExprCommonSubexpr a -> ExtractFuncExprCommonSubexpr <$> traverse extractList a
  OverlayFuncExprCommonSubexpr a -> OverlayFuncExprCommonSubexpr <$> overlayList a
  PositionFuncExprCommonSubexpr a -> PositionFuncExprCommonSubexpr <$> traverse positionList a
  SubstringFuncExprCommonSubexpr a -> SubstringFuncExprCommonSubexpr <$> traverse substrList a
  TreatFuncExprCommonSubexpr a b -> TreatFuncExprCommonSubexpr <$> aExpr a <*> typename b
  TrimFuncExprCommonSubexpr a b -> TrimFuncExprCommonSubexpr <$> traverse trimModifier a <*> trimList b
  NullIfFuncExprCommonSubexpr a b -> NullIfFuncExprCommonSubexpr <$> aExpr a <*> aExpr b
  CoalesceFuncExprCommonSubexpr a -> CoalesceFuncExprCommonSubexpr <$> exprList a
  GreatestFuncExprCommonSubexpr a -> GreatestFuncExprCommonSubexpr <$> exprList a
  LeastFuncExprCommonSubexpr a -> LeastFuncExprCommonSubexpr <$> exprList a

extractList (ExtractList a b) = ExtractList <$> extractArg a <*> aExpr b

extractArg = \case
  IdentExtractArg a -> IdentExtractArg <$> ident a
  x -> pure x

overlayList (OverlayList a b c d) = OverlayList <$> aExpr a <*> overlayPlacing b <*> substrFrom c <*> traverse substrFor d

overlayPlacing a = aExpr a

positionList (PositionList a b) = PositionList <$> bExpr a <*> bExpr b

substrList = \case
  ExprSubstrList a b -> ExprSubstrList <$> aExpr a <*> substrListFromFor b
  ExprListSubstrList a -> ExprListSubstrList <$> exprList a

substrListFromFor = \case
  FromForSubstrListFromFor a b -> FromForSubstrListFromFor <$> substrFrom a <*> substrFor b
  ForFromSubstrListFromFor a b -> ForFromSubstrListFromFor <$> substrFor a <*> substrFrom b
  FromSubstrListFromFor a -> FromSubstrListFromFor <$> substrFrom a
  ForSubstrListFromFor a -> ForSubstrListFromFor <$> substrFor a

substrFrom a = aExpr a

substrFor a = aExpr a

trimModifier = pure

trimList = \case
  ExprFromExprListTrimList a b -> ExprFromExprListTrimList <$> aExpr a <*> exprList b
  FromExprListTrimList a -> FromExprListTrimList <$> exprList a
  ExprListTrimList a -> ExprListTrimList <$> exprList a

-- * AexprConsts

aexprConst = \case
  FuncAexprConst a b c -> FuncAexprConst <$> funcName a <*> traverse funcAexprConstArgList b <*> pure c
  ConstTypenameAexprConst a b -> ConstTypenameAexprConst <$> constTypename a <*> pure b
  x -> pure x

funcAexprConstArgList (FuncConstArgs a b) = FuncConstArgs <$> traverse funcArgExpr a <*> traverse sortClause b

constTypename = \case
  NumericConstTypename a -> NumericConstTypename <$> numeric a
  ConstBitConstTypename a -> ConstBitConstTypename <$> constBit a
  x -> pure x

numeric = \case
  DecimalNumeric a -> DecimalNumeric <$> traverse exprList a
  DecNumeric a -> DecNumeric <$> traverse exprList a
  NumericNumeric a -> NumericNumeric <$> traverse exprList a
  x -> pure x

bit (Bit a b) = Bit a <$> traverse exprList b

constBit = bit

constCharacter = pure

character = pure
constDatetime = pure

timezone = pure

interval = pure
intervalSecond = pure

-- * Names and refs

columnref (Columnref a b) = Columnref <$> colId a <*> traverse indirection b

ident = pure

qualifiedName = \case
  SimpleQualifiedName a -> SimpleQualifiedName <$> ident a
  IndirectedQualifiedName a b -> IndirectedQualifiedName <$> ident a <*> indirection b

getNextParam :: Int -> Maybe Text -> State InputParams Int
getNextParam param k = do
  seenParams <- get
  case Map.lookup (param, k) seenParams of
    Nothing -> do
      let renamedParam = Map.size seenParams + 1
          newMap = Map.insert (param, k) renamedParam seenParams
      put newMap
      pure renamedParam
    Just v -> pure v

hsIndirection :: Int -> Indirection -> State InputParams (Int, Indirection)
hsIndirection param xs = case NonEmpty.head xs of
    AttrNameIndirectionEl a@(HsIdent hsIdent) -> do
      rParam <- getNextParam param (Just hsIdent)
      (rParam, ) . pure . AttrNameIndirectionEl <$> ident a
    _ -> do
      rParam <- getNextParam param Nothing
      (rParam, ) . NonEmpty.fromList <$> traverse (\case
        AttrNameIndirectionEl (HsIdent _) -> error "Cannot handle a chained haskell indirections."
        AttrNameIndirectionEl a -> AttrNameIndirectionEl <$> ident a
        AllIndirectionEl -> pure AllIndirectionEl
        ExprIndirectionEl a -> ExprIndirectionEl <$> aExpr a
        SliceIndirectionEl a b ->SliceIndirectionEl <$> traverse aExpr a <*> traverse aExpr b
        )
        (NonEmpty.tail xs)

indirection = traverse indirectionEl

indirectionEl = \case
  AttrNameIndirectionEl a -> AttrNameIndirectionEl <$> ident a
  AllIndirectionEl -> pure AllIndirectionEl
  ExprIndirectionEl a -> ExprIndirectionEl <$> aExpr a
  SliceIndirectionEl a b -> SliceIndirectionEl <$> traverse aExpr a <*> traverse aExpr b

colId = ident

name = colId

cursorName = name

colLabel = ident

attrName = colLabel

typeFunctionName = ident

funcName = \case
  TypeFuncName a -> TypeFuncName <$> typeFunctionName a
  IndirectedFuncName a b -> IndirectedFuncName <$> colId a <*> indirection b

anyName (AnyName a b) = AnyName <$> colId a <*> traverse attrs b

-- * Types

typename (Typename a b c d) = Typename a <$>
  simpleTypename b
  <*> pure c
  <*> pure d

simpleTypename = \case
  GenericTypeSimpleTypename a -> GenericTypeSimpleTypename <$> genericType a
  NumericSimpleTypename a -> NumericSimpleTypename <$> numeric a
  BitSimpleTypename a -> BitSimpleTypename <$> bit a
  x -> pure x

genericType (GenericType a b c) = GenericType <$> typeFunctionName a <*> traverse attrs b <*> traverse typeModifiers c

attrs = traverse attrName

typeModifiers = exprList

typeList = traverse typename

subType = pure

-- * Indexes

indexParams = traverse indexElem

indexElem (IndexElem a b c d e) = IndexElem <$>
  indexElemDef a
  <*> traverse collate b
  <*> traverse class_ c
  <*> traverse ascDesc d
  <*> traverse nullsOrder e

indexElemDef = \case
  IdIndexElemDef a -> IdIndexElemDef <$> colId a
  FuncIndexElemDef a -> FuncIndexElemDef <$> funcExprWindownless a
  ExprIndexElemDef a -> ExprIndexElemDef <$> aExpr a

collate = anyName

class_ = anyName

ascDesc = pure

nullsOrder = pure
