module LambdaComp.Elaborated.TypeCheck
  ( runProgramInfer

  , Context
  , TypeCheckInfo(..)
  , TypeCheckT
  , runGlobalTypeCheckT
  , runTypeCheckT
  , askTypeCheckInfo
  , asksTypeCheckInfo

  , TypeError
  ) where

import Control.Monad        (foldM, unless, when, zipWithM_)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks, local)
import Data.Bifunctor       (Bifunctor (first))
import Data.Foldable        (foldr')
import Data.Map             (Map)
import Data.Map             qualified as Map

import LambdaComp.Elaborated.Syntax
import LambdaComp.PrimOp            (PrimOpTypeBase (..), getPrimOpType)

type Context = Map Ident Tp
data TypeCheckInfo
  = TypeCheckInfo
    { topDefs :: Context
    , localCtx :: Context
    }
type TypeCheckT = ReaderT TypeCheckInfo
type TypeErrorC = MonadError TypeError

runProgramInfer :: (TypeErrorC m) => Program -> m Context
runProgramInfer p = do
  ctx <- foldM go Map.empty p
  let lastDef = tmDefName (last p)
  when (ctx Map.! lastDef /= TpInt) $ throwError $ NonIntLastTopDecl lastDef
  pure ctx
  where
    go :: (TypeErrorC m) => Context -> Top -> m (Map Ident Tp)
    go ctx top = ($ ctx) . Map.insert (tmDefName top) <$> topInfer top `runGlobalTypeCheckT` ctx

topInfer :: (TypeErrorC m) => Top -> TypeCheckT m Tp
topInfer = infer . tmDefBody

check :: (TypeErrorC m) => Tm -> Tp -> TypeCheckT m ()
check (TmConst c)              = \tp ->
  let tp' = inferConst c in
  if tp == tp'
  then pure ()
  else throwError $ InvalidConstType tp tp'
check tm                       = \tp -> do
  tp' <- infer tm
  when (tp /= tp') $
    throwError $ TypeMismatch tp tp'

infer :: (TypeErrorC m) => Tm -> TypeCheckT m Tp
infer (TmVar x)                = asksTypeCheckInfo (Map.lookup x . localCtx) >>= maybe (throwError $ NotInScope x) pure
infer (TmGlobal x)             = asksTypeCheckInfo (Map.lookup x . topDefs) >>= maybe (throwError $ NotDefined x) pure
infer (TmConst c)              = pure $ inferConst c
infer (TmIf tm0 tm1 tm2)       = do
  tp0 <- infer tm0
  case tp0 of
    TpBool -> do
      tp1 <- infer tm1
      tp2 <- infer tm2
      unless (tp1 == tp2) $
        throwError $ BranchTypeMismatch tp1 tp2
      pure tp1
    _      -> throwError $ TypeMismatch TpBool tp0
infer (TmLam ps tm)            = TpFun (fmap paramType ps) <$> local (insertParamsToInfo ps) (infer tm)
infer (tmf `TmApp` tmas)       = do
  tpf <- infer tmf
  case flattenFunctionType tpf of
    ([], _)                      -> throwError $ NonFunType tpf
    (tpPs, tpR)
      | tpPsLength == tmasLength -> tpR <$ zipWithM_ check tmas tpPs
      | tpPsLength > tmasLength  -> tpPsRest `TpFun` tpR <$ zipWithM_ check tmas tpPsUsed
      | otherwise                -> throwError $ NonFunType tpR
      where
        tpPsLength = length tpPs
        tmasLength = length tmas
        (tpPsUsed, tpPsRest) = splitAt tmasLength tpPs
infer (TmPrimBinOp op tm0 tm1) = do
  check tm0 arg0Tp
  check tm1 arg1Tp
  pure retTp
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
infer (TmPrimUnOp op tm)       = do
  check tm argTp
  pure retTp
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
infer (TmPrintInt tm0 tm1)     = do
  tp0 <- infer tm0
  case tp0 of
    TpInt -> infer tm1
    _     -> throwError $ TypeMismatch TpInt tp0
infer (TmPrintDouble tm0 tm1)  = do
  tp0 <- infer tm0
  case tp0 of
    TpDouble -> infer tm1
    _        -> throwError $ TypeMismatch TpDouble tp0
infer (TmRec p tm)             = paramType p <$ local (insertParamToInfo p) (check tm $ paramType p)

inferConst :: TmConst -> Tp
inferConst TmCUnit       = TpUnit
inferConst TmCTrue       = TpBool
inferConst TmCFalse      = TpBool
inferConst (TmCInt _)    = TpInt
inferConst (TmCDouble _) = TpDouble

flattenFunctionType :: Tp -> ([Tp], Tp)
flattenFunctionType (tpPs `TpFun` tpR) = first (tpPs <>) $ flattenFunctionType tpR
flattenFunctionType tpR                = ([], tpR)

insertParamsToInfo :: [Param] -> TypeCheckInfo -> TypeCheckInfo
insertParamsToInfo ps info = info{ localCtx = foldr' insertParamToContext (localCtx info) ps }

insertParamToInfo :: Param -> TypeCheckInfo -> TypeCheckInfo
insertParamToInfo p info = info{ localCtx = insertParamToContext p $ localCtx info }

insertParamToContext :: Param -> Context -> Context
insertParamToContext Param {..} = Map.insert paramName paramType

primOpTypeBase :: PrimOpTypeBase Tp
primOpTypeBase = PrimOpTypeBase { boolTp = TpBool, intTp = TpInt, doubleTp = TpDouble }

runGlobalTypeCheckT :: TypeCheckT m a -> Context -> m a
runGlobalTypeCheckT tc = runTypeCheckT tc . flip TypeCheckInfo Map.empty

runTypeCheckT :: TypeCheckT m a -> TypeCheckInfo -> m a
runTypeCheckT = runReaderT

askTypeCheckInfo :: (Monad m) => TypeCheckT m TypeCheckInfo
askTypeCheckInfo = ask

asksTypeCheckInfo :: (Monad m) => (TypeCheckInfo -> a) -> TypeCheckT m a
asksTypeCheckInfo = asks

data TypeError where
  NonIntLastTopDecl  :: Ident -> TypeError
  NotInScope         :: Ident -> TypeError
  NotDefined         :: Ident -> TypeError
  TypeMismatch       :: Tp -> Tp -> TypeError
  BranchTypeMismatch :: Tp -> Tp -> TypeError
  InvalidConstType   :: Tp -> Tp -> TypeError
  NonFunType         :: Tp -> TypeError
  NeedTypeAnn        :: Tm -> TypeError
  deriving stock Show
