module LambdaComp.CBPV.Optimization.Local
  ( runLocalOptDefault
  ) where

import LambdaComp.CBPV.Optimization.BindingConversion
import LambdaComp.CBPV.Optimization.SkipReturn
import LambdaComp.CBPV.Syntax

runLocalOptDefault :: Program -> Program
runLocalOptDefault = fmap runLocalOptDefaultTop

runLocalOptDefaultTop :: Top -> Top
runLocalOptDefaultTop m = m{ tmDefBody = runLocalOptDefaultTm $ tmDefBody m }

runLocalOptDefaultTm :: Tm Val -> Tm Val
runLocalOptDefaultTm = runLiftingLet . runSkipReturn . runCommutingThen
