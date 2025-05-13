{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module LambdaComp.External.ToElaborated
  ( runToElaborated

  , ElaborationError
  ) where

import Control.Monad                  (unless, when, zipWithM)
import Control.Monad.Except           (MonadError (throwError, catchError))
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

type Context = Map Ident XTp
type ToElaborated = ReaderT (Context, Ident) (WriterT Any (Either ElaborationError))

runToElaborated :: Program -> Either ElaborationError E.Program
runToElaborated = go Map.empty
  where
    go _   [] = pure []
    go ctx ((TopTmDecl x xtp, _) : (TopTmDef x' xtm, _) : prog)
      | x == x' = do
        (top, _) <- runWriterT $ topCheck x xtm xtp `runReaderT` (Map.insert x xtp ctx, x)
        (top :) <$> go (addTop top xtp ctx) prog
    go _   ((TopTmDecl x _, _) : _) = throwError $ InvalidTopDecl x
    go ctx ((TopTmDef x xtm, defSpan) : prog) = do
      ((top, tp), _) <- runWriterT $ topInfer x xtm defSpan `runReaderT` (ctx, x)
      (top :) <$> go (addTop top tp ctx) prog

    addTop :: E.Top -> XTp -> Context -> Context
    addTop E.TopTmDef {..} = Map.insert tmDefName

topCheck :: Ident -> XTm -> XTp -> ToElaborated E.Top
topCheck tmDefName xtm (tmDefType, _) = do
  (tm', Any referSelf) <- listen $ xcheck xtm tmDefType
  let
    p = E.Param tmDefName tmDefType
    tmDefBody
      | referSelf = E.TmRec p tm'
      | otherwise = tm'
  pure E.TopTmDef {..}

topInfer :: Ident -> XTm -> SourceSpan -> ToElaborated (E.Top, XTp)
topInfer tmDefName xtm topSpan = do
  ((tp, tm'), Any referSelf) <- listen $ xinfer xtm
  let
    p = E.Param tmDefName tp
    tmDefBody
      | referSelf = E.TmRec p tm'
      | otherwise = tm'
  pure (E.TopTmDef {..}, (tp, topSpan))

xcheck :: XTm -> Tp -> ToElaborated E.Tm
xcheck (tm, tmSpan) = wrapErrorWithSpan tmSpan . check tm

check :: Tm -> Tp -> ToElaborated E.Tm
check TmUnit                     = \case
  TpUnit    -> pure E.TmUnit
  tp        -> throwError $ InvalidConsType tp [TpUnit]
check TmTrue                     = \case
  TpBool -> pure E.TmTrue
  tp     -> throwError $ InvalidConsType tp [TpBool]
check TmFalse                    = \case
  TpBool -> pure E.TmFalse
  tp     -> throwError $ InvalidConsType tp [TpBool]
check (TmInt i)                  = \case
  TpInt -> pure (E.TmInt i)
  tp    -> throwError $ InvalidConsType tp [TpInt]
check (TmDouble d)               = \case
  TpDouble -> pure (E.TmDouble d)
  tp       -> throwError $ InvalidConsType tp [TpDouble]
check (TmIf xtm0 xtm1 xtm2)      = \tp -> do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    TpBool -> E.TmIf tm0' <$> xcheck xtm1 tp <*> xcheck xtm2 tp
    _      -> throwError $ TypeMismatch TpBool tp0
check (TmLam xps xtm)            = \case
  tpPs `TpFun` tpR -> do
    bs <- zipWithM xcheckParam xps tpPs
    E.TmLam (uncurry E.Param . second fst <$> bs) <$> local (first $ Map.union (Map.fromList bs)) (xcheck xtm tpR)
  tp               -> do
    name <- asks snd
    throwError $ traceShow name $ NonFunType tp
check (TmPrimBinOp op xtm0 xtm1) = \tp -> do
  unless (tp == retTp) $
    throwError $ TypeMismatch retTp tp
  E.TmPrimBinOp op <$> xcheck xtm0 arg0Tp <*> xcheck xtm1 arg1Tp
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
check (TmPrimUnOp op xtm)        = \tp -> do
  unless (tp == retTp) $
    throwError $ TypeMismatch retTp tp
  E.TmPrimUnOp op <$> xcheck xtm argTp
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
check (TmPrintInt xtm0 xtm1)     = \tp -> do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    TpInt -> E.TmPrintInt tm0' <$> xcheck xtm1 tp
    _     -> throwError $ TypeMismatch TpInt tp0
check tm                       = \tp -> do
  (tp', tm') <- infer tm
  when (tp /= tp') $
    throwError $ TypeMismatch tp tp'
  pure tm'

xinfer :: XTm -> ToElaborated (Tp, E.Tm)
xinfer (tm, tmSpan) = wrapErrorWithSpan tmSpan (infer tm)

infer :: Tm -> ToElaborated (Tp, E.Tm)
infer (xtm `TmAnn` xtp)          = (fst xtp,) <$> xcheck xtm (fst xtp)
infer (TmVar x)                  = do
  currentFun <- asks snd
  tell (Any (currentFun == x))
  asks (Map.lookup x . fst) >>= maybe (throwError $ NotInScope x) (pure . (, E.TmVar x) . fst)
infer TmUnit                     = pure (TpUnit, E.TmUnit)
infer TmTrue                     = pure (TpBool, E.TmTrue)
infer TmFalse                    = pure (TpBool, E.TmFalse)
infer (TmInt i)                  = pure (TpInt, E.TmInt i)
infer (TmDouble d)               = pure (TpDouble, E.TmDouble d)
infer (TmIf xtm0 xtm1 xtm2)      = do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    TpBool -> do
      (tp1, tm1') <- xinfer xtm1
      (tp2, tm2') <- xinfer xtm2
      if tp1 == tp2
        then pure (tp1, E.TmIf tm0' tm1' tm2')
        else throwError $ BranchTypeMismatch tp1 tp2
    _      -> throwError $ TypeMismatch TpBool tp0
infer (TmLam xps xtm)            = do
  bs <- traverse xinferParam xps
  tpR <- local (first $ Map.union (Map.fromList bs)) (xinfer xtm)
  pure
    $ bimap
      (TpFun (fst . snd <$> bs))
      (E.TmLam (uncurry E.Param . second fst <$> bs))
      tpR
infer (xtmf `TmApp` xtmas)       = do
  (tpf, tmf') <- xinfer xtmf
  case tpf of
    tpPs `TpFun` tpR -> (tpR ,) . E.TmApp tmf' <$> zipWithM xcheck xtmas tpPs
    _                -> throwError $ NonFunType tpf
infer (TmPrimBinOp op xtm0 xtm1) =
  (retTp,) <$> liftA2 (E.TmPrimBinOp op) (xcheck xtm0 arg0Tp) (xcheck xtm1 arg1Tp)
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
infer (TmPrimUnOp op xtm)        =
  (retTp,) . E.TmPrimUnOp op <$> xcheck xtm argTp
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
infer (TmPrintInt xtm0 xtm1)     = do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    TpInt -> second (E.TmPrintInt tm0') <$> xinfer xtm1
    _     -> throwError $ TypeMismatch TpInt tp0
infer (TmPrintDouble xtm0 xtm1)  = do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    TpDouble -> second (E.TmPrintDouble tm0') <$> xinfer xtm1
    _        -> throwError $ TypeMismatch TpDouble tp0

xcheckParam :: XParam -> Tp -> ToElaborated (Ident, XTp)
xcheckParam (param, paramSpan) tp = wrapErrorWithSpan paramSpan $ second (, paramSpan) <$> checkParam param tp

checkParam :: Param -> Tp -> ToElaborated (Ident, Tp)
checkParam (Param x mayTpP) tpP' = do
  case mayTpP of
    Just tpP -> unless (tpP == tpP') $
      throwError $ TypeMismatch tpP' tpP
    Nothing  -> pure ()
  pure (x, tpP')

xinferParam :: XParam -> ToElaborated (Ident, XTp)
xinferParam (param, paramSpan) = wrapErrorWithSpan paramSpan $ second (, paramSpan) <$> inferParam param

inferParam :: Param -> ToElaborated (Ident, Tp)
inferParam (Param x mayTpP) = do
  case mayTpP of
    Just tpP -> pure (x, tpP)
    Nothing  -> throwError $ NeedParamTypeAnn x

primOpTypeBase :: PrimOpTypeBase Tp
primOpTypeBase = PrimOpTypeBase { boolTp = TpBool, intTp = TpInt, doubleTp = TpDouble }

wrapErrorWithSpan :: SourceSpan -> ToElaborated a -> ToElaborated a
wrapErrorWithSpan s m = catchError m $ throwError . (`OfSpan` s)

data ElaborationError where
  InvalidTopDecl     :: !Ident -> ElaborationError
  NotInScope         :: !Ident -> ElaborationError
  TypeMismatch       :: Tp -> Tp -> ElaborationError
  BranchTypeMismatch :: Tp -> Tp -> ElaborationError
  InvalidConsType    :: Tp -> [Tp] -> ElaborationError
  NonFunType         :: Tp -> ElaborationError
  NeedParamTypeAnn   :: !Ident -> ElaborationError
  OfSpan             :: ElaborationError -> SourceSpan -> ElaborationError
  deriving stock Show
