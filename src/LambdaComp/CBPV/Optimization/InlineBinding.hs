{-# LANGUAGE RecursiveDo #-}
module LambdaComp.CBPV.Optimization.InlineBinding
    ( runInlineSimpleLet
    , runInlineLinearLet
    ) where

import Control.Monad.Reader       (MonadReader (local), Reader, asks, runReader)
import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (fromMaybe)

import LambdaComp.Binder      (getBoundVar, lensBinderBody)
import LambdaComp.CBPV.Syntax

runInlineSimpleLet :: Tm c -> Tm c
runInlineSimpleLet = (`runReader` Map.empty) . inlineSimpleLet

type WithSimpleBinding = Reader (Map Ident (Tm Val))

inlineSimpleLet :: Tm c -> WithSimpleBinding (Tm c)
inlineSimpleLet tm@(TmGlobal _
                   ; TmConst _
                   ; TmThunk _) = polyRecTmM inlineSimpleLet tm
inlineSimpleLet tm@(TmVar x)    = do
  mayTm' <- asks (Map.!? x)
  pure $ fromMaybe tm mayTm'

inlineSimpleLet tm@(TmIf {}
                   ; TmLam _; TmApp {}
                   ; TmForce _
                   ; TmReturn _; TmTo {}
                   ; TmPrimBinOp {}; TmPrimUnOp {}
                   ; TmPrintInt {}; TmPrintDouble {}
                   ; TmRec _) = polyRecTmBM inlineSimpleLet inlineSimpleLetBinder tm
inlineSimpleLet (TmLet tm0 b) = liftA2 TmLet (inlineSimpleLet tm0) (lensBinderBody (local (Map.insert x xBinding) . inlineSimpleLet) b)
  where
    x = getBoundVar b
    xBinding
      | isSimpleTm tm0 = tm0
      | otherwise      = TmVar x

    isSimpleTm :: Tm Val -> Bool
    isSimpleTm (TmVar _; TmGlobal _; TmConst _) = True
    isSimpleTm (TmThunk _)                      = False

inlineSimpleLetBinder :: Binder t -> WithSimpleBinding (Binder t)
inlineSimpleLetBinder b = lensBinderBody (local (Map.insert x $ TmVar x) . inlineSimpleLet) b
  where
    x = getBoundVar b

runInlineLinearLet :: Tm c -> Tm c
runInlineLinearLet = (`evalState` Map.empty) . inlineLinearLet

type WithLinearBinding = State (Map Ident (Tm Val, Int))

inlineLinearLet :: Tm c -> WithLinearBinding (Tm c)
inlineLinearLet tm@(TmGlobal _
                   ; TmConst _
                   ; TmThunk _) = polyRecTmM inlineLinearLet tm
inlineLinearLet tm@(TmVar x)    = do
  mayTm' <- gets (Map.!? x)
  case mayTm' of
    Just (tm', n) -> tm' <$ modify' (Map.insert x (tm', n + 1))
    Nothing       -> pure tm

inlineLinearLet tm@(TmIf {}
                   ; TmLam _; TmApp {}
                   ; TmForce _
                   ; TmReturn _; TmTo {}
                   ; TmPrimBinOp {}; TmPrimUnOp {}
                   ; TmPrintInt {}; TmPrintDouble {}
                   ; TmRec _)                = polyRecTmBM inlineLinearLet inlineLinearLetBinder tm
inlineLinearLet (TmLet tm0 (BUntyped x tm1)) = do
  tm0' <- inlineLinearLet tm0
  prevV <- gets (Map.!? x)
  rec
    modify' $ Map.insert x (if doubleUsed then TmVar x else tm0', 0)
    tm1' <- inlineLinearLet tm1
    doubleUsed <- gets $ (> 1) . snd . (Map.! x)
    modify' (Map.update (const prevV) x)
  pure $ TmLet tm0' (BUntyped x tm1')

inlineLinearLetBinder :: Binder t -> WithLinearBinding (Binder t)
inlineLinearLetBinder b = lensBinderBody (withSelfBinding x . inlineLinearLet) b
  where
    x = getBoundVar b

withSelfBinding :: Ident -> WithLinearBinding a -> WithLinearBinding a
withSelfBinding x m = do
  prevV <- gets (Map.!? x)
  modify' . Map.insert x $ (TmVar x, 0)
  m <* modify' (Map.update (const prevV) x)
