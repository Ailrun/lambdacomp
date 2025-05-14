{-# LANGUAGE GADTs #-}
module LambdaComp.CBPV.Optimization.DeadBindingElimination
  ( runDeadLetElimination
  ) where

import Control.Monad.Writer.CPS (MonadWriter (tell), Writer, censor, runWriter, listens)
import Data.Set                 (Set)
import Data.Set                 qualified as Set

import LambdaComp.CBPV.Syntax
import Control.Applicative (liftA3)
import Data.Functor ((<&>))

runDeadLetElimination :: Tm Val -> Tm Val
runDeadLetElimination = fst . runWriter . deadLetElimination

type WithFreeVars = Writer (Set Ident)

deadLetElimination :: Tm c -> WithFreeVars (Tm c)
deadLetElimination tm@(TmVar x)             = tm <$ tell (Set.singleton x)
deadLetElimination tm@TmUnit                = pure tm
deadLetElimination tm@TmTrue                = pure tm
deadLetElimination tm@TmFalse               = pure tm
deadLetElimination tm@(TmInt _)             = pure tm
deadLetElimination tm@(TmDouble _)          = pure tm
deadLetElimination (TmThunk tm)             = TmThunk <$> deadLetElimination tm
deadLetElimination (TmIf tm0 tm1 tm2)       = liftA3 TmIf (deadLetElimination tm0) (deadLetElimination tm1) (deadLetElimination tm2)
deadLetElimination (TmLam p tm)             = TmLam p <$> without (paramName p) (deadLetElimination tm)
deadLetElimination (tmf `TmApp` tma)        = liftA2 TmApp (deadLetElimination tmf) (deadLetElimination tma)
deadLetElimination (TmForce tm)             = TmForce <$> deadLetElimination tm
deadLetElimination (TmReturn tm)            = TmReturn <$> deadLetElimination tm
deadLetElimination (TmTo tm0 x tm1)         = liftA3 TmTo (deadLetElimination tm0) (pure x) (without x $ deadLetElimination tm1)
deadLetElimination (TmLet x tm0 tm1)        = do
  (tm1', withX) <- without x $ listens (x `Set.member`) $ deadLetElimination tm1
  if withX
    then TmLet x <$> deadLetElimination tm0 <&> ($ tm1')
    else pure tm1'
deadLetElimination (TmPrimBinOp op tm0 tm1) = liftA2 (TmPrimBinOp op) (deadLetElimination tm0) (deadLetElimination tm1)
deadLetElimination (TmPrimUnOp op tm)       = TmPrimUnOp op <$> deadLetElimination tm
deadLetElimination (TmPrintInt tm0 tm1)     = liftA2 TmPrintInt (deadLetElimination tm0) (deadLetElimination tm1)
deadLetElimination (TmPrintDouble tm0 tm1)  = liftA2 TmPrintDouble (deadLetElimination tm0) (deadLetElimination tm1)
deadLetElimination (TmRec p tm)             = TmRec p <$> without (paramName p) (deadLetElimination tm)

without :: Ident -> WithFreeVars a -> WithFreeVars a
without x = censor (Set.delete x)
