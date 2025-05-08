{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module LambdaComp.External.ToElaborated
  ( runToElaborated

  , ElaborationError
  ) where

import Control.Monad                  (unless, when, zipWithM)
import Control.Monad.Except           (MonadError (throwError))
import Control.Monad.Reader           (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Writer.CPS       (MonadWriter (listen, tell))
import Data.Bifunctor                 (Bifunctor (first, second, bimap))
import Data.Map                       (Map)
import Data.Map                       qualified as Map
import Data.Semigroup                 (Any (Any))

import LambdaComp.Elaborated.Syntax qualified as E
import LambdaComp.External.Syntax
import LambdaComp.PrimOp            (PrimOpTypeBase (..), getPrimOpType)
import Debug.Trace (traceShow)

type Context = Map Ident Tp
type ToElaborated = ReaderT (Context, Ident) (WriterT Any (Either ElaborationError))

runToElaborated :: Program -> Either ElaborationError E.Program
runToElaborated = go Map.empty
  where
    go _   [] = pure []
    go ctx (TopTmDecl x tp : TopTmDef x' tm : prog)
      | x == x' = do
          (top, _) <- runWriterT $ topCheck x tm tp `runReaderT` (Map.insert x tp ctx, x)
          (top :) <$> go (addTop top tp ctx) prog
    go _   (TopTmDecl x _ : _) = throwError $ InvalidTopDecl x
    go ctx (TopTmDef x tm : prog) = do
          ((top, tp), _) <- runWriterT $ topInfer x tm `runReaderT` (ctx, x)
          (top :) <$> go (addTop top tp ctx) prog

    addTop :: E.Top -> E.Tp -> Context -> Context
    addTop E.TopTmDef {..} = Map.insert tmDefName

topCheck :: Ident -> Tm -> Tp -> ToElaborated E.Top
topCheck tmDefName tm tmDefType = do
  (tm', Any referSelf) <- listen $ check tm tmDefType
  let
    tmDefBody
      | referSelf = E.TmRec tmDefName tmDefType tm'
      | otherwise = tm'
  pure E.TopTmDef {..}

topInfer :: Ident -> Tm -> ToElaborated (E.Top, E.Tp)
topInfer tmDefName tm = do
  ((tp, tm'), Any referSelf) <- listen $ infer tm
  let
    tmDefBody
      | referSelf = E.TmRec tmDefName tp tm'
      | otherwise = tm'
  pure (E.TopTmDef {..}, tp)

check :: Tm -> Tp -> ToElaborated E.Tm
check TmUnit                   = \case
  TpUnit    -> pure E.TmUnit
  tp        -> throwError $ InvalidConsType tp [TpUnit]
check TmTrue                   = \case
  TpBool -> pure E.TmTrue
  tp     -> throwError $ InvalidConsType tp [TpBool]
check TmFalse                  = \case
  TpBool -> pure E.TmFalse
  tp     -> throwError $ InvalidConsType tp [TpBool]
check (TmInt i)                = \case
  TpInt -> pure (E.TmInt i)
  tp    -> throwError $ InvalidConsType tp [TpInt]
check (TmDouble d)             = \case
  TpDouble -> pure (E.TmDouble d)
  tp       -> throwError $ InvalidConsType tp [TpDouble]
check (TmIf tm0 tm1 tm2)       = \tp -> do
  (tp0, tm0') <- infer tm0
  case tp0 of
    TpBool -> E.TmIf tm0' <$> check tm1 tp <*> check tm2 tp
    _      -> throwError $ TypeMismatch TpBool tp0
check (TmLam ps tm)            = \case
  tpPs `TpFun` tpR -> do
    bs <- zipWithM checkParam ps tpPs
    E.TmLam (uncurry E.Param <$> bs) <$> local (first $ Map.union (Map.fromList bs)) (check tm tpR)
  tp               -> do
    name <- asks snd
    throwError $ traceShow name $ NonFunType tp
check (TmPrimBinOp op tm0 tm1) = \tp -> do
  unless (tp == retTp) $
    throwError $ TypeMismatch retTp tp
  E.TmPrimBinOp op <$> check tm0 arg0Tp <*> check tm1 arg1Tp
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
check (TmPrimUnOp op tm)       = \tp -> do
  unless (tp == retTp) $
    throwError $ TypeMismatch retTp tp
  E.TmPrimUnOp op <$> check tm argTp
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
check (TmPrintInt tm0 tm1)     = \tp -> do
  (tp0, tm0') <- infer tm0
  case tp0 of
    TpInt -> E.TmPrintInt tm0' <$> check tm1 tp
    _     -> throwError $ TypeMismatch TpInt tp0
check tm                       = \tp -> do
  (tp', tm') <- infer tm
  when (tp /= tp') $
    throwError $ TypeMismatch tp tp'
  pure tm'

infer :: Tm -> ToElaborated (Tp, E.Tm)
infer (tm `TmAnn` tp)          = (tp,) <$> check tm tp
infer (TmVar x)                = do
  currentFun <- asks snd
  tell (Any (currentFun == x))
  asks (Map.lookup x . fst) >>= maybe (throwError $ NotInScope x) (pure . (, E.TmVar x))
infer TmUnit                   = pure (TpUnit, E.TmUnit)
infer TmTrue                   = pure (TpBool, E.TmTrue)
infer TmFalse                  = pure (TpBool, E.TmFalse)
infer (TmInt i)                = pure (TpInt, E.TmInt i)
infer (TmDouble d)             = pure (TpDouble, E.TmDouble d)
infer (TmIf tm0 tm1 tm2)       = do
  (tp0, tm0') <- infer tm0
  case tp0 of
    TpBool -> do
      (tp1, tm1') <- infer tm1
      (tp2, tm2') <- infer tm2
      if tp1 == tp2
        then pure (tp1, E.TmIf tm0' tm1' tm2')
        else throwError $ BranchTypeMismatch tp1 tp2
    _      -> throwError $ TypeMismatch TpBool tp0
infer (TmLam ps tm)            = do
  bs <- traverse inferParam ps
  bimap (TpFun (snd <$> bs)) (E.TmLam (uncurry E.Param <$> bs)) <$> local (first $ Map.union (Map.fromList bs)) (infer tm)
infer (tmf `TmApp` tmas)       = do
  (tpf, tmf') <- infer tmf
  case tpf of
    tpPs `TpFun` tpR -> (tpR ,) . E.TmApp tmf' <$> zipWithM check tmas tpPs
    _                -> throwError $ NonFunType tpf
infer (TmPrimBinOp op tm0 tm1) =
  (retTp,) <$> liftA2 (E.TmPrimBinOp op) (check tm0 arg0Tp) (check tm1 arg1Tp)
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
infer (TmPrimUnOp op tm)       =
  (retTp,) . E.TmPrimUnOp op <$> check tm argTp
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
infer (TmPrintInt tm0 tm1)     = do
  (tp0, tm0') <- infer tm0
  case tp0 of
    TpInt -> second (E.TmPrintInt tm0') <$> infer tm1
    _     -> throwError $ TypeMismatch TpInt tp0

checkParam :: Param -> Tp -> ToElaborated (Ident, Tp)
checkParam (Param x mayTpP) tpP' = do
  case mayTpP of
    Just tpP -> unless (tpP == tpP') $
      throwError $ TypeMismatch tpP' tpP
    Nothing  -> pure ()
  pure (x, tpP')

inferParam :: Param -> ToElaborated (Ident, Tp)
inferParam (Param x mayTpP) = do
  case mayTpP of
    Just tpP -> pure (x, tpP)
    Nothing  -> throwError $ NeedParamTypeAnn x

primOpTypeBase :: PrimOpTypeBase Tp
primOpTypeBase = PrimOpTypeBase { boolTp = TpBool, intTp = TpInt, doubleTp = TpDouble }

data ElaborationError where
  InvalidTopDecl     :: !Ident -> ElaborationError
  NotInScope         :: !Ident -> ElaborationError
  TypeMismatch       :: Tp -> Tp -> ElaborationError
  BranchTypeMismatch :: Tp -> Tp -> ElaborationError
  InvalidConsType    :: Tp -> [Tp] -> ElaborationError
  NonFunType         :: Tp -> ElaborationError
  NeedParamTypeAnn   :: !Ident -> ElaborationError
  deriving stock Show
