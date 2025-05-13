module LambdaComp.Elaborated.Optimization.Local
  ( runLocalOptDefault
  ) where

import LambdaComp.Elaborated.Optimization.ConstantPropagation
import LambdaComp.Elaborated.Syntax

runLocalOptDefault :: Program -> Program
runLocalOptDefault = fmap runLocalOptDefaultTop

runLocalOptDefaultTop :: Top -> Top
runLocalOptDefaultTop m = m{ tmDefBody = runLocalOptDefaultTm $ tmDefBody m }

runLocalOptDefaultTm :: Tm -> Tm
runLocalOptDefaultTm = runConstantsPropagation
