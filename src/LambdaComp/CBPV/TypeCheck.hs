{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module LambdaComp.CBPV.TypeCheck
  ( topCheck
  , topInfer

  , TypeError
  ) where

import Control.Monad        (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Data.Map             (Map)
import Data.Map             qualified as Map

import LambdaComp.CBPV.Syntax
import LambdaComp.PrimOp      (PrimOpTypeBase (..), getPrimOpType)

type TypeCheck = ReaderT (Map Ident (Tp Val)) (Either TypeError)

topCheck :: Tm c -> Tp c -> Either TypeError ()
topCheck tm = (`runReaderT` initialContext) . check tm
  where
    initialContext = Map.empty

topInfer :: Tm c -> Either TypeError (Tp c)
topInfer = (`runReaderT` initialContext) . infer
  where
    initialContext = Map.empty

check :: Tm c -> Tp c -> TypeCheck ()
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
  tp        -> throwError $ NonDownType tp
check (TmTo tm0 x tm1)         = \tp -> do
  tp0 <- infer tm0
  case tp0 of
    TpDown tp0' -> local (Map.insert x tp0') $ check tm1 tp
    _           -> throwError $ NonDownType tp0
check (TmLet x tm0 tm1)        = \tp -> do
  tp0 <- infer tm0
  local (Map.insert x tp0) $ check tm1 tp
check tm                       = \tp -> do
  tp' <- infer tm
  when (tp /= tp') $
    throwError $ TypeMismatch tp tp'

infer :: Tm c -> TypeCheck (Tp c)
infer (TmVar x)                = asks (Map.lookup x) >>= maybe (throwError $ NotInScope x) pure
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
    _      -> throwError $ TypeMismatch TpBool tpc
infer (TmLam p tm)             = TpFun (paramType p) <$> local (Map.insert (paramName p) (paramType p)) (infer tm)
infer (tmf `TmApp` tma)        = do
  tpf <- infer tmf
  case tpf of
    tp0 `TpFun` tp1 -> tp1 <$ check tma tp0
    _               -> throwError $ NonFunType tpf
infer (TmForce tm)             = do
  tp <- infer tm
  case tp of
    TpUp tp' -> pure tp'
    _        -> throwError $ NonUpType tp
infer (TmReturn tm)            = TpDown <$> infer tm
infer (TmTo tm0 x tm1)         = do
  tp0 <- infer tm0
  case tp0 of
    TpDown tp0' -> local (Map.insert x tp0') $ infer tm1
    _           -> throwError $ NonDownType tp0
infer (TmLet x tm0 tm1)        = do
  tp0 <- infer tm0
  local (Map.insert x tp0) $ infer tm1
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
    _     -> throwError $ TypeMismatch TpInt tp0
infer (TmPrintDouble tm0 tm1)  = do
  tp0 <- infer tm0
  case tp0 of
    TpDouble -> infer tm1
    _        -> throwError $ TypeMismatch TpDouble tp0
infer (TmRec x tp tm)          = TpDown tp <$ local (Map.insert x tp) (check tm (TpDown tp))

primOpTypeBase :: PrimOpTypeBase (Tp Val)
primOpTypeBase = PrimOpTypeBase { boolTp = TpBool, intTp = TpInt, doubleTp = TpDouble }

data TypeError where
  NotInScope         :: Ident -> TypeError
  TypeMismatch       :: Tp c -> Tp c -> TypeError
  BranchTypeMismatch :: Tp c -> Tp c -> TypeError
  InvalidConsType    :: Tp Val -> [Tp Val] -> TypeError
  NonFunType         :: Tp Com -> TypeError
  NonUpType          :: Tp Val -> TypeError
  NonDownType        :: Tp Com -> TypeError

deriving stock instance Show TypeError
