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

import Control.Monad        (foldM, unless, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks, local)
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
  when (ctx Map.! lastDef /= TpConst TpCInt) $ throwError $ NonIntLastTopDecl lastDef
  pure ctx
  where
    go :: (TypeErrorC m) => Context -> Top -> m (Map Ident Tp)
    go ctx top = flip (Map.insert (tmDefName top)) ctx <$> topInfer top `runGlobalTypeCheckT` ctx

topInfer :: (TypeErrorC m) => Top -> TypeCheckT m Tp
topInfer = infer . tmDefBody

check :: (TypeErrorC m) => Tm -> Tp -> TypeCheckT m ()
check (TmConst c)              = \tp ->
  let tp' = TpConst $ inferConst c in
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
infer (TmConst c)              = pure . TpConst $ inferConst c
infer (TmIf tm0 tm1 tm2)       = do
  tp0 <- infer tm0
  case tp0 of
    TpConst TpCBool -> do
      tp1 <- infer tm1
      tp2 <- infer tm2
      unless (tp1 == tp2) $
        throwError $ BranchTypeMismatch tp1 tp2
      pure tp1
    _               -> throwError $ TypeMismatch (TpConst TpCBool) tp0
infer (TmLam p tm)             = TpFun (paramType p) <$> local (insertParamToInfo p) (infer tm)
infer (tmf `TmApp` tma)        = do
  tpf <- infer tmf
  case tpf of
    tpP `TpFun` tpR -> tpR <$ check tma tpP
    _               -> throwError $ NonFunType tpf
infer (TmPrimBinOp op tm0 tm1) = do
  check tm0 $ TpConst arg0Tp
  check tm1 $ TpConst arg1Tp
  pure $ TpConst retTp
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
infer (TmPrimUnOp op tm)       = do
  check tm $ TpConst argTp
  pure $ TpConst retTp
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
infer (TmPrintInt tm0 tm1)     = do
  tp0 <- infer tm0
  case tp0 of
    TpConst TpCInt -> infer tm1
    _              -> throwError $ TypeMismatch (TpConst TpCInt) tp0
infer (TmPrintDouble tm0 tm1)  = do
  tp0 <- infer tm0
  case tp0 of
    TpConst TpCDouble -> infer tm1
    _                 -> throwError $ TypeMismatch (TpConst TpCDouble) tp0
infer (TmRec p tm)             = paramType p <$ local (insertParamToInfo p) (check tm $ paramType p)

inferConst :: TmConst -> TpConst
inferConst TmCUnit       = TpCUnit
inferConst TmCTrue       = TpCBool
inferConst TmCFalse      = TpCBool
inferConst (TmCInt _)    = TpCInt
inferConst (TmCDouble _) = TpCDouble

insertParamToInfo :: Param -> TypeCheckInfo -> TypeCheckInfo
insertParamToInfo p info = info{ localCtx = insertParamToContext p $ localCtx info }

insertParamToContext :: Param -> Context -> Context
insertParamToContext Param {..} = Map.insert paramName paramType

primOpTypeBase :: PrimOpTypeBase TpConst
primOpTypeBase = PrimOpTypeBase { boolTp = TpCBool, intTp = TpCInt, doubleTp = TpCDouble }

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
