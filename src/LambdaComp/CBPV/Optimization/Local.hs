module LambdaComp.CBPV.Optimization.Local
  ( runLocalOptDefault
  ) where

import LambdaComp.CBPV.Optimization.BindingConversion      (runCommutingTo, runLiftingLet)
import LambdaComp.CBPV.Optimization.DeadBindingElimination (runDeadLetElimination)
import LambdaComp.CBPV.Optimization.InlineBinding          (runInlineSimpleLet)
import LambdaComp.CBPV.Optimization.SkipReturn             (runSkipReturn)
import LambdaComp.CBPV.Syntax

runLocalOptDefault :: Program -> Program
runLocalOptDefault = fmap runLocalOptDefaultTop

runLocalOptDefaultTop :: Top -> Top
runLocalOptDefaultTop m = m{ tmDefBody = runLocalOptDefaultTm $ tmDefBody m }

runLocalOptDefaultTm :: Tm Com -> Tm Com
runLocalOptDefaultTm = runDeadLetElimination . runInlineSimpleLet . runLiftingLet . runSkipReturn . runCommutingTo
