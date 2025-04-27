module LambdaComp.CBPV.LocalOptimization
  ( runLocalOptDefault
  ) where

import LambdaComp.CBPV.BindingConversion
import LambdaComp.CBPV.SkipReturn
import LambdaComp.CBPV.Syntax

runLocalOptDefault :: Program -> Program
runLocalOptDefault = fmap runLocalOptDefaultTop

runLocalOptDefaultTop :: Top -> Top
runLocalOptDefaultTop m = m{ tmDefBody = runLocalOptDefaultTm $ tmDefBody m }

runLocalOptDefaultTm :: Tm Val -> Tm Val
runLocalOptDefaultTm = runLiftingLet . runSkipReturn . runCommutingThen
