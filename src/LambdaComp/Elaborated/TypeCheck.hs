{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module LambdaComp.Elaborated.TypeCheck
  ( runProgramInfer

  , Context
  , TypeError
  ) where

import Control.Monad        (foldM, unless, when, zipWithM_)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (ReaderT (runReaderT), asks, local)
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
type TypeCheck = ReaderT TypeCheckInfo (Either TypeError)

runProgramInfer :: Program -> Either TypeError Context
runProgramInfer = foldM go Map.empty
  where
    go ctx top = ($ ctx) . Map.insert (tmDefName top) <$> topInfer top `runReaderT` TypeCheckInfo ctx Map.empty

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
infer (TmVar x)                = asks (Map.lookup x . localCtx) >>= maybe (throwError $ NotInScope x) pure
infer (TmGlobal x)             = asks (Map.lookup x . topDefs) >>= maybe (throwError $ NotDefined x) pure
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

data TypeError where
  NotInScope         :: Ident -> TypeError
  NotDefined         :: Ident -> TypeError
  TypeMismatch       :: Tp -> Tp -> TypeError
  BranchTypeMismatch :: Tp -> Tp -> TypeError
  InvalidConsType    :: Tp -> [Tp] -> TypeError
  NonFunType         :: Tp -> TypeError
  NeedTypeAnn        :: Tm -> TypeError
  deriving stock Show
