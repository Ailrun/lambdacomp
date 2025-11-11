{-# LANGUAGE ViewPatterns #-}
module LambdaComp.External.ToElaborated
  ( runToElaborated

  , ElaborationError
  ) where

import Control.Applicative            (Alternative ((<|>)))
import Control.Monad                  (unless, when, zipWithM)
import Control.Monad.Except           (MonadError (catchError, throwError))
import Control.Monad.Reader           (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Writer.CPS       (MonadWriter (listen, tell))
import Data.Bifunctor                 (Bifunctor (bimap, first, second))
import Data.Map                       (Map)
import Data.Map                       qualified as Map
import Data.Semigroup                 (Any (Any))

import LambdaComp.Elaborated.Syntax qualified as E
import LambdaComp.External.Syntax
import LambdaComp.PrimOp            (PrimOpTypeBase (..), getPrimOpType)

type Context = Map Ident E.Tp
data ToElaboratedInfo
  = ToElaboratedInfo
    { topDefs :: Context
    , localCtx :: Context
    , currentDef :: Ident
    }
type ToElaborated = ReaderT ToElaboratedInfo (WriterT Any (Either ElaborationError))

------------------------------------------------------------
-- Program Elaboration

runToElaborated :: Program -> Either ElaborationError E.Program
runToElaborated = go Map.empty
  where
    go :: Context -> Program -> Either ElaborationError E.Program
    go _   [] = pure []
    go ctx ((TopTmDecl x xtp, _) : (TopTmDef x' xtm, _) : prog)
      | x == x' = do
          when (null prog && fst xtp /= TpConst TpCInt) $ throwError $ NonIntLastTopDecl x
          (top, _) <- runWriterT $ topCheck x xtm xtp `runReaderT` ToElaboratedInfo ctx (Map.singleton x (xtpElaborate xtp)) x
          (top :) <$> go (addTop x (xtpElaborate xtp) ctx) prog
    go _   ((TopTmDecl x _, _) : _) = throwError $ InvalidTopDecl x
    go ctx ((TopTmDef x xtm, _) : prog) = do
      ((top, tp), _) <- runWriterT $ topInfer x xtm `runReaderT` ToElaboratedInfo ctx Map.empty x
      when (null prog && (tp /= E.TpConst E.TpCInt)) $ throwError $ NonIntLastTopDecl x
      (top :) <$> go (addTop x tp ctx) prog

    addTop :: Ident -> E.Tp -> Context -> Context
    addTop = Map.insert

------------------------------------------------------------
-- Top-level Elaboration

topCheck :: Ident -> XTm -> XTp -> ToElaborated E.Top
topCheck (toExtVar -> tmDefName) xtm (tpElaborate -> tmDefType, _) = do
  (tm', Any referSelf) <- listen $ xcheck xtm tmDefType
  let
    p = E.Param tmDefName tmDefType
    tmDefBody
      | referSelf = E.TmRec p tm'
      | otherwise = tm'
  pure E.TopTmDef {..}

topInfer :: Ident -> XTm -> ToElaborated (E.Top, E.Tp)
topInfer (toExtVar -> tmDefName) xtm = do
  ((tp, tm'), Any referSelf) <- listen $ xinfer xtm
  let
    p = E.Param tmDefName tp
    tmDefBody
      | referSelf = E.TmRec p tm'
      | otherwise = tm'
  pure (E.TopTmDef {..}, tp)

------------------------------------------------------------
-- Type Elaboration

xtpElaborate :: XTp -> E.Tp
xtpElaborate = tpElaborate . fst

tpElaborate :: Tp -> E.Tp
tpElaborate (TpConst tpc)      = E.TpConst $ tpConstElaborate tpc
tpElaborate (tpPs `TpFun` tpR) = foldr (\p acc -> tpElaborate p `E.TpFun` acc) (tpElaborate tpR) tpPs

tpConstElaborate :: TpConst -> E.TpConst
tpConstElaborate TpCUnit   = E.TpCUnit
tpConstElaborate TpCBool   = E.TpCBool
tpConstElaborate TpCInt    = E.TpCInt
tpConstElaborate TpCDouble = E.TpCDouble

------------------------------------------------------------
-- Term Elaboration

xcheck :: XTm -> E.Tp -> ToElaborated E.Tm
xcheck (tm, tmSpan) = wrapErrorWithSpan tmSpan . check tm

check :: Tm -> E.Tp -> ToElaborated E.Tm
check (TmConst c)                = \tp ->
  let (E.TpConst -> tp', tm') = inferConst c in
  if tp == tp
  then pure $ E.TmConst tm'
  else throwError $ InvalidConstType tp tp'
check (TmIf xtm0 xtm1 xtm2)      = \tp -> do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    E.TpConst E.TpCBool -> E.TmIf tm0' <$> xcheck xtm1 tp <*> xcheck xtm2 tp
    _                   -> throwError $ TypeMismatch (E.TpConst E.TpCBool) tp0
check (TmLam xps xtm)            = \(flattenFunctionType -> (tpPs, tpR)) ->
  case tpPs of
    []                          -> throwError $ NonFunType tpR
    _
      | tpPsLength == xpsLength -> do
          bs <- zipWithM xcheckParam xps tpPs
          foldr (E.TmLam . uncurry E.Param . first toExtVar) <$> local (addLocalCtx (Map.fromList bs)) (xcheck xtm tpR) <*> pure bs
      | tpPsLength > xpsLength  -> do
          bs <- zipWithM xcheckParam xps tpPsUsed
          foldr (E.TmLam . uncurry E.Param . first toExtVar) <$> local (addLocalCtx (Map.fromList bs)) (xcheck xtm (foldr E.TpFun tpR tpPsRest)) <*> pure bs
      | otherwise               -> throwError $ NonFunType tpR
      where
        tpPsLength = length tpPs
        xpsLength = length xps
        (tpPsUsed, tpPsRest) = splitAt xpsLength tpPs
check (TmPrimBinOp op xtm0 xtm1) = \tp -> do
  unless (tp == E.TpConst retTp) $
    throwError $ TypeMismatch (E.TpConst retTp) tp
  E.TmPrimBinOp op <$> xcheck xtm0 (E.TpConst arg0Tp) <*> xcheck xtm1 (E.TpConst arg1Tp)
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
check (TmPrimUnOp op xtm)        = \tp -> do
  unless (tp == E.TpConst retTp) $
    throwError $ TypeMismatch (E.TpConst retTp) tp
  E.TmPrimUnOp op <$> xcheck xtm (E.TpConst argTp)
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
check (TmPrintInt xtm0 xtm1)     = \tp -> do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    E.TpConst E.TpCInt -> E.TmPrintInt tm0' <$> xcheck xtm1 tp
    _                  -> throwError $ TypeMismatch (E.TpConst E.TpCInt) tp0
check tm                       = \tp -> do
  (tp', tm') <- infer tm
  when (tp /= tp') $
    throwError $ TypeMismatch tp tp'
  pure tm'

xinfer :: XTm -> ToElaborated (E.Tp, E.Tm)
xinfer (tm, tmSpan) = wrapErrorWithSpan tmSpan (infer tm)

infer :: Tm -> ToElaborated (E.Tp, E.Tm)
infer (xtm `TmAnn` xtp)          = (xtpElaborate xtp,) <$> xcheck xtm (xtpElaborate xtp)
infer (TmVar x)                  = do
  current <- asks currentDef
  tell (Any (current == x))
  let x' = toExtVar x
  mayLocalX <- asks
    (\d -> (fmap (, E.TmVar x') . Map.lookup x $ localCtx d)
           <|> (fmap (, E.TmGlobal x') . Map.lookup x $ topDefs d))
  maybe (throwError $ NotInScope x) pure mayLocalX
infer (TmConst c)                = pure (bimap E.TpConst E.TmConst $ inferConst c)
infer (TmIf xtm0 xtm1 xtm2)      = do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    E.TpConst E.TpCBool -> do
      (tp1, tm1') <- xinfer xtm1
      (tp2, tm2') <- xinfer xtm2
      if tp1 == tp2
        then pure (tp1, E.TmIf tm0' tm1' tm2')
        else throwError $ BranchTypeMismatch tp1 tp2
    _                   -> throwError $ TypeMismatch (E.TpConst E.TpCBool) tp0
infer (TmLam xps xtm)            = do
  bs <- traverse xinferParam xps
  bimap
    (flip (foldr E.TpFun) (snd <$> bs))
    (flip (foldr (E.TmLam . uncurry E.Param . first toExtVar)) bs)
    <$> local (addLocalCtx (Map.fromList bs)) (xinfer xtm)
infer (xtmf `TmApp` xtmas)       = do
  (tpf, tmf') <- xinfer xtmf
  case flattenFunctionType tpf of
    ([], _)                       -> throwError $ NonFunType tpf
    (tpPs, tpR)
      | tpPsLength == xtmasLength -> (tpR,) . foldl E.TmApp tmf' <$> zipWithM xcheck xtmas tpPs
      | tpPsLength > xtmasLength  -> (foldr E.TpFun tpR tpPsRest,) . foldl E.TmApp tmf' <$> zipWithM xcheck xtmas tpPsUsed
      | otherwise                 -> throwError $ NonFunType tpR
      where
        tpPsLength = length tpPs
        xtmasLength = length xtmas
        (tpPsUsed, tpPsRest) = splitAt xtmasLength tpPs
infer (TmPrimBinOp op xtm0 xtm1) =
  (E.TpConst retTp,) <$> liftA2 (E.TmPrimBinOp op) (xcheck xtm0 (E.TpConst arg0Tp)) (xcheck xtm1 (E.TpConst arg1Tp))
  where
    ((arg0Tp, arg1Tp), retTp) = getPrimOpType op primOpTypeBase
infer (TmPrimUnOp op xtm)        =
  (E.TpConst retTp,) . E.TmPrimUnOp op <$> xcheck xtm (E.TpConst argTp)
  where
    (argTp, retTp) = getPrimOpType op primOpTypeBase
infer (TmPrintInt xtm0 xtm1)     = do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    E.TpConst E.TpCInt -> second (E.TmPrintInt tm0') <$> xinfer xtm1
    _                  -> throwError $ TypeMismatch (E.TpConst E.TpCInt) tp0
infer (TmPrintDouble xtm0 xtm1)  = do
  (tp0, tm0') <- xinfer xtm0
  case tp0 of
    E.TpConst E.TpCDouble -> second (E.TmPrintDouble tm0') <$> xinfer xtm1
    _                     -> throwError $ TypeMismatch (E.TpConst E.TpCDouble) tp0

inferConst :: TmConst -> (E.TpConst, E.TmConst)
inferConst TmCUnit       = (E.TpCUnit, E.TmCUnit)
inferConst TmCTrue       = (E.TpCBool, E.TmCTrue)
inferConst TmCFalse      = (E.TpCBool, E.TmCFalse)
inferConst (TmCInt i)    = (E.TpCInt, E.TmCInt i)
inferConst (TmCDouble d) = (E.TpCDouble, E.TmCDouble d)

flattenFunctionType :: E.Tp -> ([E.Tp], E.Tp)
flattenFunctionType (tpP `E.TpFun` tpR) = first (tpP :) $ flattenFunctionType tpR
flattenFunctionType tpR                 = ([], tpR)

xcheckParam :: XParam -> E.Tp -> ToElaborated (Ident, E.Tp)
xcheckParam (param, paramSpan) tp = wrapErrorWithSpan paramSpan $ checkParam param tp

checkParam :: Param -> E.Tp -> ToElaborated (Ident, E.Tp)
checkParam (Param x mayTpP) tpP' = do
  case mayTpP of
    Just (tpElaborate -> tpP) -> unless (tpP == tpP') $
      throwError $ TypeMismatch tpP' tpP
    Nothing                   -> pure ()
  pure (x, tpP')

xinferParam :: XParam -> ToElaborated (Ident, E.Tp)
xinferParam (param, paramSpan) = wrapErrorWithSpan paramSpan $ inferParam param

inferParam :: Param -> ToElaborated (Ident, E.Tp)
inferParam (Param x mayTpP) = do
  case mayTpP of
    Just tpP -> pure (x, tpElaborate tpP)
    Nothing  -> throwError $ NeedParamTypeAnn x

primOpTypeBase :: PrimOpTypeBase E.TpConst
primOpTypeBase = PrimOpTypeBase { boolTp = E.TpCBool, intTp = E.TpCInt, doubleTp = E.TpCDouble }

addLocalCtx :: Context -> ToElaboratedInfo -> ToElaboratedInfo
addLocalCtx newctx info = info{ localCtx = Map.union newctx $ localCtx info }

------------------------------------------------------------
-- Elaboration Errors

wrapErrorWithSpan :: SourceSpan -> ToElaborated a -> ToElaborated a
wrapErrorWithSpan s m = catchError m $ throwError . (`OfSpan` s)

data ElaborationError where
  NonIntLastTopDecl  :: !Ident -> ElaborationError
  InvalidTopDecl     :: !Ident -> ElaborationError
  NotInScope         :: !Ident -> ElaborationError
  TypeMismatch       :: E.Tp -> E.Tp -> ElaborationError
  BranchTypeMismatch :: E.Tp -> E.Tp -> ElaborationError
  InvalidConstType   :: E.Tp -> E.Tp -> ElaborationError
  NonFunType         :: E.Tp -> ElaborationError
  NeedParamTypeAnn   :: !Ident -> ElaborationError
  OfSpan             :: ElaborationError -> SourceSpan -> ElaborationError
  deriving stock Show
