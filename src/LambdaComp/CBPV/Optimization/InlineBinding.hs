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
inlineSimpleLet tm@(TmGlobal _
                   ; TmConst _
                   ; TmThunk _) = polyRecTmM inlineSimpleLet tm
inlineSimpleLet tm@(TmVar x)    = do
  mayTm' <- asks (Map.!? x)
  pure $ fromMaybe tm mayTm'

inlineSimpleLet tm@(TmIf {}
                   ; TmApp {}
                   ; TmForce {}
                   ; TmReturn {}
                   ; TmPrimBinOp {}
                   ; TmPrimUnOp {}
                   ; TmPrintInt {}
                   ; TmPrintDouble {}) = polyRecTmM inlineSimpleLet tm
inlineSimpleLet (TmLam p tm)           = TmLam p <$> local (Map.insert (paramName p) (TmVar (paramName p))) (inlineSimpleLet tm)
inlineSimpleLet (TmTo tm0 x tm1)       = liftA3 TmTo (inlineSimpleLet tm0) (pure x) (local (Map.insert x (TmVar x)) $ inlineSimpleLet tm1)
inlineSimpleLet (TmLet x tm0 tm1)      = liftA2 (TmLet x) (inlineSimpleLet tm0) (local (Map.insert x xBinding) $ inlineSimpleLet tm1)
  where
    xBinding
      | isSimpleTm tm0 = tm0
      | otherwise      = TmVar x

    isSimpleTm :: Tm Val -> Bool
    isSimpleTm (TmVar _; TmGlobal _; TmConst _) = True
    isSimpleTm (TmThunk _)                      = False
inlineSimpleLet (TmRec p tm)           = TmRec p <$> local (Map.insert (paramName p) (TmVar (paramName p))) (inlineSimpleLet tm)


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
                   ; TmApp {}
                   ; TmForce {}
                   ; TmReturn {}
                   ; TmPrimBinOp {}
                   ; TmPrimUnOp {}
                   ; TmPrintInt {}
                   ; TmPrintDouble {}) = polyRecTmM inlineLinearLet tm
inlineLinearLet (TmLam p tm)           = TmLam p <$> withSelfBinding (paramName p) (inlineLinearLet tm)
inlineLinearLet (TmTo tm0 x tm1)       = liftA2 (`TmTo` x) (inlineLinearLet tm0) (withSelfBinding x $ inlineLinearLet tm1)
inlineLinearLet (TmLet x tm0 tm1)      = do
  tm0' <- inlineLinearLet tm0
  prevV <- gets (Map.!? x)
  rec
    modify' $ Map.insert x (if doubleUsed then TmVar x else tm0', 0)
    tm1' <- inlineLinearLet tm1
    doubleUsed <- gets $ (> 1) . snd . (Map.! x)
    modify' (Map.update (const prevV) x)
  pure $ TmLet x tm0' tm1'
inlineLinearLet (TmRec p tm)           = TmRec p <$> withSelfBinding (paramName p) (inlineLinearLet tm)

withSelfBinding :: Ident -> WithLinearBinding a -> WithLinearBinding a
withSelfBinding x m = do
  prevV <- gets (Map.!? x)
  modify' . Map.insert x $ (TmVar x, 0)
  m <* modify' (Map.update (const prevV) x)
