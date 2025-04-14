{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaComp.CBPV.Optimization where

import LambdaComp.CBPV.BindingConversion
import LambdaComp.CBPV.Syntax
import LambdaComp.CBPV.SkipReturn

topOptimizeDefault :: Tm 'Com -> Tm 'Com
topOptimizeDefault = topLiftingLet . topSkipReturn . topCommutingThen
