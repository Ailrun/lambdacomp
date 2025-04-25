{-# LANGUAGE LambdaCase #-}
module LambdaComp.TypeCheck
  ( topCheck
  , topInfer

  , TypeError
  ) where

import Control.Monad        (unless, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Data.Map             (Map)
import Data.Map             qualified as Map

import LambdaComp.PrimOp (PrimOpTypeBase (..), getPrimOpType)
import LambdaComp.Syntax

type TypeCheck = ReaderT (Map Ident Tp) (Either TypeError)

topCheck :: Tm -> Tp -> Either TypeError ()
topCheck tm = (`runReaderT` initialContext) . check tm
  where
    initialContext = Map.empty

topInfer :: Tm -> Either TypeError Tp
topInfer = (`runReaderT` initialContext) . infer
  where
    initialContext = Map.empty

check :: Tm -> Tp -> TypeCheck ()
check TmUnit                   = \case
  TpUnit    -> pure ()
  tp        -> throwError $ InvalidConsType tp [TpUnit]
check TmTrue                   = \case
  TpBool -> pure ()
  tp     -> throwError $ InvalidConsType tp [TpBool]
check TmFalse                  = \case
  TpBool -> pure ()
  tp     -> throwError $ InvalidConsType tp [TpBool]
check (TmInt _)                = \case
  TpInt -> pure ()
  tp    -> throwError $ InvalidConsType tp [TpInt]
check (TmDouble _)             = \case
  TpDouble -> pure ()
  tp       -> throwError $ InvalidConsType tp [TpDouble]
check (TmIf tm0 tm1 tm2)       = \tp -> do
  tp0 <- infer tm0
  case tp0 of
    TpBool -> check tm1 tp >> check tm2 tp
    _      -> throwError $ TypeMismatch TpBool tp0
check (TmLam x tm)             = \case
  tp0 `TpFun` tp1 -> local (Map.insert x tp0) $ check tm tp1
  tp              -> throwError $ NonFunType tp
check (TmPrimBinOp op tm0 tm1) = \tp -> do
  unless (tp == retTp) $
    throwError $ TypeMismatch retTp tp
  check tm0 arg0Tp
  check tm1 arg1Tp
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
check (TmPrimUnOp op tm)       = \tp -> do
  unless (tp == retTp) $
    throwError $ TypeMismatch retTp tp
  check tm argTp
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
check (TmPrintInt tm0 tm1)     = \tp -> do
  tp0 <- infer tm0
  case tp0 of
    TpInt -> check tm1 tp
    _     -> throwError $ TypeMismatch TpInt tp0
check (TmRec f tm)             = \tp -> local (Map.insert f tp) $ check tm tp
check tm                       = \tp -> do
  tp' <- infer tm
  when (tp /= tp') $
    throwError $ TypeMismatch tp tp'

infer :: Tm -> TypeCheck Tp
infer (tm `TmAnn` tp)          = tp <$ check tm tp
infer (TmVar x)                = asks (Map.lookup x) >>= maybe (throwError $ NotInScope x) pure
infer TmUnit                   = pure TpUnit
infer TmTrue                   = pure TpBool
infer TmFalse                  = pure TpBool
infer (TmInt _)                = pure TpInt
infer (TmDouble _)             = pure TpDouble
infer (TmIf tm0 tm1 tm2)       = do
  tp0 <- infer tm0
  case tp0 of
    TpBool -> do
      tp1 <- infer tm1
      tp2 <- infer tm2
      if tp1 == tp2
        then pure tp1
        else throwError $ BranchTypeMismatch tp1 tp2
    _      -> throwError $ TypeMismatch TpBool tp0
infer (tmf `TmApp` tma)        = do
  tpf <- infer tmf
  case tpf of
    tp0 `TpFun` tp1 -> tp1 <$ check tma tp0
    _               -> throwError $ NonFunType tpf
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
infer tm                       = throwError $ NeedTypeAnn tm

primOpTypeBase :: PrimOpTypeBase Tp
primOpTypeBase = PrimOpTypeBase { boolTp = TpBool, intTp = TpInt, doubleTp = TpDouble }

data TypeError where
  NotInScope         :: Ident -> TypeError
  TypeMismatch       :: Tp -> Tp -> TypeError
  BranchTypeMismatch :: Tp -> Tp -> TypeError
  InvalidConsType    :: Tp -> [Tp] -> TypeError
  NonFunType         :: Tp -> TypeError
  NeedTypeAnn        :: Tm -> TypeError
  deriving stock Show
