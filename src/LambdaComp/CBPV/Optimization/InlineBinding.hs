{-# LANGUAGE RecursiveDo #-}
module LambdaComp.CBPV.Optimization.InlineBinding
    ( runInlineSimpleLet
    , runInlineLinearLet
    ) where

import Control.Applicative        (liftA3)
import Control.Monad.Reader       (MonadReader (local), Reader, asks, runReader)
import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (fromMaybe)

import LambdaComp.CBPV.Syntax

runInlineSimpleLet :: Tm c -> Tm c
runInlineSimpleLet = (`runReader` Map.empty) . inlineSimpleLet

type WithSimpleBinding = Reader (Map Ident (Tm Val))

inlineSimpleLet :: Tm c -> WithSimpleBinding (Tm c)
inlineSimpleLet tm@(TmVar x)             = do
  mayTm' <- asks (Map.!? x)
  pure $ fromMaybe tm mayTm'
inlineSimpleLet tm@(TmGlobal _)          = pure tm
inlineSimpleLet tm@TmUnit                = pure tm
inlineSimpleLet tm@TmTrue                = pure tm
inlineSimpleLet tm@TmFalse               = pure tm
inlineSimpleLet tm@(TmInt _)             = pure tm
inlineSimpleLet tm@(TmDouble _)          = pure tm
inlineSimpleLet (TmThunk tm)             = TmThunk <$> inlineSimpleLet tm
inlineSimpleLet (TmIf tm0 tm1 tm2)       = liftA3 TmIf (inlineSimpleLet tm0) (inlineSimpleLet tm1) (inlineSimpleLet tm2)
inlineSimpleLet (TmLam p tm)             = TmLam p <$> local (Map.insert (paramName p) (TmVar (paramName p))) (inlineSimpleLet tm)
inlineSimpleLet (tmf `TmApp` tma)        = liftA2 TmApp (inlineSimpleLet tmf) (inlineSimpleLet tma)
inlineSimpleLet (TmForce tm)             = TmForce <$> inlineSimpleLet tm
inlineSimpleLet (TmReturn tm)            = TmReturn <$> inlineSimpleLet tm
inlineSimpleLet (TmTo tm0 x tm1)         = liftA3 TmTo (inlineSimpleLet tm0) (pure x) (local (Map.insert x (TmVar x)) $ inlineSimpleLet tm1)
inlineSimpleLet (TmLet x tm0 tm1)        = liftA2 (TmLet x) (inlineSimpleLet tm0) (local (Map.insert x xBinding) $ inlineSimpleLet tm1)
  where
    xBinding
      | isSimpleTm x tm0 = tm0
      | otherwise        = TmVar x
inlineSimpleLet (TmPrimBinOp op tm0 tm1) = liftA2 (TmPrimBinOp op) (inlineSimpleLet tm0) (inlineSimpleLet tm1)
inlineSimpleLet (TmPrimUnOp op tm)       = TmPrimUnOp op <$> inlineSimpleLet tm
inlineSimpleLet (TmPrintInt tm0 tm1)     = liftA2 TmPrintInt (inlineSimpleLet tm0) (inlineSimpleLet tm1)
inlineSimpleLet (TmPrintDouble tm0 tm1)  = liftA2 TmPrintDouble (inlineSimpleLet tm0) (inlineSimpleLet tm1)
inlineSimpleLet (TmRec p tm)             = TmRec p <$> local (Map.insert (paramName p) (TmVar (paramName p))) (inlineSimpleLet tm)

isSimpleTm :: Ident -> Tm Val -> Bool
isSimpleTm x (TmVar y)    = x /= y
isSimpleTm _ (TmGlobal _) = True
isSimpleTm _ TmUnit       = True
isSimpleTm _ TmTrue       = True
isSimpleTm _ TmFalse      = True
isSimpleTm _ (TmInt _)    = True
isSimpleTm _ (TmDouble _) = True
isSimpleTm _ (TmThunk _)  = False

runInlineLinearLet :: Tm c -> Tm c
runInlineLinearLet = (`evalState` Map.empty) . inlineLinearLet

type WithLinearBinding = State (Map Ident (Tm Val, Int))

inlineLinearLet :: Tm c -> WithLinearBinding (Tm c)
inlineLinearLet tm@(TmVar x)             = do
  mayTm' <- gets (Map.!? x)
  case mayTm' of
    Just (tm', n) -> tm' <$ modify' (Map.insert x (tm', n + 1))
    Nothing       -> pure tm
inlineLinearLet tm@(TmGlobal _)          = pure tm
inlineLinearLet tm@TmUnit                = pure tm
inlineLinearLet tm@TmTrue                = pure tm
inlineLinearLet tm@TmFalse               = pure tm
inlineLinearLet tm@(TmInt _)             = pure tm
inlineLinearLet tm@(TmDouble _)          = pure tm
inlineLinearLet (TmThunk tm)             = TmThunk <$> inlineLinearLet tm
inlineLinearLet (TmIf tm0 tm1 tm2)       = liftA3 TmIf (inlineLinearLet tm0) (inlineLinearLet tm1) (inlineLinearLet tm2)
inlineLinearLet (TmLam p tm)             = TmLam p <$> withSelfBinding (paramName p) (inlineLinearLet tm)
inlineLinearLet (tmf `TmApp` tma)        = liftA2 TmApp (inlineLinearLet tmf) (inlineLinearLet tma)
inlineLinearLet (TmForce tm)             = TmForce <$> inlineLinearLet tm
inlineLinearLet (TmReturn tm)            = TmReturn <$> inlineLinearLet tm
inlineLinearLet (TmTo tm0 x tm1)         = liftA2 (`TmTo` x) (inlineLinearLet tm0) (withSelfBinding x $ inlineLinearLet tm1)
inlineLinearLet (TmLet x tm0 tm1)        = do
  tm0' <- inlineLinearLet tm0
  prevV <- gets (Map.!? x)
  rec
    modify' $ Map.insert x (if doubleUsed then TmVar x else tm0', 0)
    tm1' <- inlineLinearLet tm1
    doubleUsed <- gets $ (> 1) . snd . (Map.! x)
    modify' (Map.update (const prevV) x)
  pure $ TmLet x tm0' tm1'
inlineLinearLet (TmPrimBinOp op tm0 tm1) = liftA2 (TmPrimBinOp op) (inlineLinearLet tm0) (inlineLinearLet tm1)
inlineLinearLet (TmPrimUnOp op tm)       = TmPrimUnOp op <$> inlineLinearLet tm
inlineLinearLet (TmPrintInt tm0 tm1)     = liftA2 TmPrintInt (inlineLinearLet tm0) (inlineLinearLet tm1)
inlineLinearLet (TmPrintDouble tm0 tm1)  = liftA2 TmPrintDouble (inlineLinearLet tm0) (inlineLinearLet tm1)
inlineLinearLet (TmRec p tm)             = TmRec p <$> withSelfBinding (paramName p) (inlineLinearLet tm)

withSelfBinding :: Ident -> WithLinearBinding a -> WithLinearBinding a
withSelfBinding x m = do
  prevV <- gets (Map.!? x)
  modify' . Map.insert x $ (TmVar x, 0)
  m <* modify' (Map.update (const prevV) x)
