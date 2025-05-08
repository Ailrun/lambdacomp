{-# LANGUAGE LambdaCase #-}
module LambdaComp.Elaborated.TypeCheck
  ( runProgramInfer

  , TypeError
  ) where

import Control.Monad        (foldM, unless, when, zipWithM_)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (ReaderT (runReaderT), asks, local)
import Data.Map             (Map)
import Data.Map             qualified as Map

import LambdaComp.Elaborated.Syntax
import LambdaComp.PrimOp            (PrimOpTypeBase (..), getPrimOpType)

type Context = Map Ident Tp
type TypeCheck = ReaderT Context (Either TypeError)

runProgramInfer :: Program -> Either TypeError Context
runProgramInfer = foldM go Map.empty
  where
    go ctx top = ($ ctx) . Map.insert (tmDefName top) <$> topInfer top `runReaderT` ctx

topInfer :: Top -> TypeCheck Tp
topInfer = infer . tmDefBody

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
check tm                       = \tp -> do
  tp' <- infer tm
  when (tp /= tp') $
    throwError $ TypeMismatch tp tp'

infer :: Tm -> TypeCheck Tp
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
      unless (tp1 == tp2) $
        throwError $ BranchTypeMismatch tp1 tp2
      pure tp1
    _      -> throwError $ TypeMismatch TpBool tp0
infer (TmLam ps tm)            = TpFun (fmap paramType ps) <$> infer tm
infer (tmf `TmApp` tmas)       = do
  tpf <- infer tmf
  case tpf of
    tpPs `TpFun` tpR -> tpR <$ zipWithM_ check tmas tpPs
    _                -> throwError $ NonFunType tpf
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
infer (TmRec x tp tm)          = tp <$ local (Map.insert x tp) (check tm tp)

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
