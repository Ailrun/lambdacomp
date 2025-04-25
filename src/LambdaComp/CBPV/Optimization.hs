module LambdaComp.CBPV.Optimization
  ( topOptimizeDefault
  ) where

import LambdaComp.CBPV.BindingConversion
import LambdaComp.CBPV.SkipReturn
import LambdaComp.CBPV.Syntax

topOptimizeDefault :: Tm Com -> Tm Com
topOptimizeDefault = topLiftingLet . topSkipReturn . topCommutingThen
