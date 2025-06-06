module LambdaComp.CBPV.TypeCheck
  ( runProgramInfer

  , Context
  , TypeCheckInfo(..)
  , TypeCheckT
  , runGlobalTypeCheckT
  , runTypeCheckT
  , askTypeCheckInfo
  , asksTypeCheckInfo

  , TypeError(..)
  ) where

import Control.Monad        (foldM, when)
import Control.Monad.Except (MonadError (throwError), withError)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Data.Map             (Map)
import Data.Map             qualified as Map
import Data.Text            (Text)

import LambdaComp.CBPV.Syntax
import LambdaComp.PrimOp      (PrimOpTypeBase (..), getPrimOpType)

type Context = Map Ident (Tp Val)
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
    go :: (TypeErrorC m) => Context -> Top -> m Context
    go ctx top = ($ ctx) . Map.insert (tmDefName top) <$> topInfer top `runGlobalTypeCheckT` ctx

topInfer :: (TypeErrorC m) => Top -> TypeCheckT m (Tp Val)
topInfer top = do
  tp <- withError (OfTop (tmDefName top)) . infer . tmDefBody $ top
  case tp of
    TpDown tp' -> pure tp'
    _          -> throwError $ NonDownType "top-level definition" tp

check :: (TypeErrorC m) => Tm c -> Tp c -> TypeCheckT m ()
check TmUnit                   = \case
  TpUnit    -> pure ()
  tp        -> throwError $ InvalidConsType tp [TpUnit]
check TmTrue                   = \case
  TpBool    -> pure ()
  tp        -> throwError $ InvalidConsType tp [TpBool]
check TmFalse                  = \case
  TpBool    -> pure ()
  tp        -> throwError $ InvalidConsType tp [TpBool]
check (TmInt _)                = \case
  TpInt -> pure ()
  tp    -> throwError $ InvalidConsType tp [TpInt]
check (TmDouble _)             = \case
  TpDouble -> pure ()
  tp       -> throwError $ InvalidConsType tp [TpDouble]
check (TmThunk tm)             = \case
  TpUp tp -> check tm tp
  tp      -> throwError $ NonUpType tp
check (TmReturn tm)            = \case
  TpDown tp -> check tm tp
  tp        -> throwError $ NonDownType "Return" tp
check (TmTo tm0 x tm1)         = \tp -> do
  tp0 <- infer tm0
  case tp0 of
    TpDown tp0' -> local (insertEntryToInfo x tp0') $ check tm1 tp
    _           -> throwError $ NonDownType "To" tp0
check (TmLet x tm0 tm1)        = \tp -> do
  tp0 <- infer tm0
  local (insertEntryToInfo x tp0) $ check tm1 tp
check tm                       = \tp -> do
  tp' <- infer tm
  when (tp /= tp') $
    throwError $ TypeMismatch "An inferable expression" tp tp'

infer :: (TypeErrorC m) => Tm c -> TypeCheckT m (Tp c)
infer (TmVar x)                = asksTypeCheckInfo (Map.lookup x . localCtx) >>= maybe (throwError $ NotInScope x) pure
infer (TmGlobal x)             = asksTypeCheckInfo (Map.lookup x . topDefs) >>= maybe (throwError $ NotDefined x) pure
infer TmUnit                   = pure TpUnit
infer TmTrue                   = pure TpBool
infer TmFalse                  = pure TpBool
infer (TmInt _)                = pure TpInt
infer (TmDouble _)             = pure TpDouble
infer (TmThunk tm)             = TpUp <$> infer tm
infer (TmIf tm0 tm1 tm2)       = do
  tpc <- infer tm0
  case tpc of
    TpBool -> do
      tp1 <- infer tm1
      tp2 <- infer tm2
      if tp1 == tp2
        then pure tp1
        else throwError $ BranchTypeMismatch tp1 tp2
    _      -> throwError $ TypeMismatch "If condition" TpBool tpc
infer (TmLam p tm)             = (paramType p :->:) <$> local (insertParamToInfo p) (infer tm)
infer (tmf `TmApp` tma)        = do
  tpf <- infer tmf
  case tpf of
    tp0 :->: tp1 -> tp1 <$ check tma tp0
    _            -> throwError $ NonFunType tpf
infer (TmForce tm)             = do
  tp <- infer tm
  case tp of
    TpUp tp' -> pure tp'
    _        -> throwError $ NonUpType tp
infer (TmReturn tm)            = TpDown <$> infer tm
infer (TmTo tm0 x tm1)         = do
  tp0 <- infer tm0
  case tp0 of
    TpDown tp0' -> local (insertEntryToInfo x tp0') $ infer tm1
    _           -> throwError $ NonDownType "To" tp0
infer (TmLet x tm0 tm1)        = do
  tp0 <- infer tm0
  local (insertEntryToInfo x tp0) $ infer tm1
infer (TmPrimBinOp op tm0 tm1) = do
  check tm0 arg0Tp
  check tm1 arg1Tp
  pure $ TpDown retTp
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
infer (TmPrimUnOp op tm)       = do
  check tm argTp
  pure $ TpDown retTp
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
infer (TmPrintInt tm0 tm1)     = do
  tp0 <- infer tm0
  case tp0 of
    TpInt -> infer tm1
    _     -> throwError $ TypeMismatch "PrintInt printing value" TpInt tp0
infer (TmPrintDouble tm0 tm1)  = do
  tp0 <- infer tm0
  case tp0 of
    TpDouble -> infer tm1
    _        -> throwError $ TypeMismatch "PrintDouble printing value" TpDouble tp0
infer (TmRec p tm)             =
  withError (OfRec (paramName p)) $ case paramType p of
    TpUp tp -> tp <$ local (insertParamToInfo p) (check tm tp)
    tp      -> throwError $ NonUpType tp

insertParamToInfo :: Param -> TypeCheckInfo -> TypeCheckInfo
insertParamToInfo p info = info{ localCtx = insertParamToContext p $ localCtx info }

insertEntryToInfo :: Ident -> Tp Val -> TypeCheckInfo -> TypeCheckInfo
insertEntryToInfo x tp info = info{ localCtx = Map.insert x tp $ localCtx info }

insertParamToContext :: Param -> Context -> Context
insertParamToContext Param {..} = Map.insert paramName paramType

primOpTypeBase :: PrimOpTypeBase (Tp Val)
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
  TypeMismatch       :: Text -> Tp c -> Tp c -> TypeError
  BranchTypeMismatch :: Tp c -> Tp c -> TypeError
  InvalidConsType    :: Tp Val -> [Tp Val] -> TypeError
  NonFunType         :: Tp Com -> TypeError
  NonUpType          :: Tp Val -> TypeError
  NonDownType        :: Text -> Tp Com -> TypeError
  OfTop              :: Ident -> TypeError -> TypeError
  OfRec              :: Ident -> TypeError -> TypeError

deriving stock instance Show TypeError
