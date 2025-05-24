module LambdaComp.CBPV.Optimization.Local
  ( runLocalOptDefault
  ) where

import Control.Monad.Identity (Identity (Identity, runIdentity))

import LambdaComp.CBPV.Optimization.BetaReduction          (runBetaReduction)
import LambdaComp.CBPV.Optimization.BindingConversion      (runCommutingTo, runLiftingLet)
import LambdaComp.CBPV.Optimization.DeadBindingElimination (runDeadLetElimination)
import LambdaComp.CBPV.Optimization.EtaReduction           (runEtaReduction)
import LambdaComp.CBPV.Optimization.InlineBinding          (runInlineLinearLet, runInlineSimpleLet)
import LambdaComp.CBPV.Syntax

runLocalOptDefault :: Program -> Program
runLocalOptDefault = fmap runLocalOptDefaultTop

runLocalOptDefaultTop :: Top -> Top
runLocalOptDefaultTop m = m{ tmDefBody = runLocalOptDefaultTm $ tmDefBody m }

runLocalOptDefaultTm :: Tm Com -> Tm Com
runLocalOptDefaultTm = runIdentity . repeatUntilFix (Identity . runDeadLetElimination . runInlineLinearLet . runInlineSimpleLet . runLiftingLet . runEtaReduction . runBetaReduction . runCommutingTo)

repeatUntilFix :: (Monad m, Eq a) => (a -> m a) -> a -> m a
repeatUntilFix f a = do
  a' <- f a
  if a == a'
    then pure a'
    else repeatUntilFix f a'
