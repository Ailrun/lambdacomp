{-# LANGUAGE GADTs #-}
module LambdaComp.CBPV.Optimization.InlineBinding
    ( runInlineSimpleLet
    ) where

import Control.Applicative  (liftA3)
import Control.Monad.Reader (MonadReader (local), Reader, asks, runReader)
import Data.Map.Strict      (Map)
import Data.Map.Strict      qualified as Map
import Data.Maybe           (fromMaybe)

import LambdaComp.CBPV.Syntax

runInlineSimpleLet :: Tm c -> Tm c
runInlineSimpleLet = (`runReader` Map.empty) . inlineSimpleLet

type WithSimpleBinding = Reader (Map Ident (Tm Val))

inlineSimpleLet :: Tm c -> WithSimpleBinding (Tm c)
inlineSimpleLet tm@(TmVar x)             = do
  mayTm' <- asks (Map.!? x)
  pure $ fromMaybe tm mayTm'
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
isSimpleTm _ TmUnit       = True
isSimpleTm _ TmTrue       = True
isSimpleTm _ TmFalse      = True
isSimpleTm _ (TmInt _)    = True
isSimpleTm _ (TmDouble _) = True
isSimpleTm _ (TmThunk _)  = False
