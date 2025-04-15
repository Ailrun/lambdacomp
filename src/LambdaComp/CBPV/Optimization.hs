{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaComp.CBPV.Optimization where

import LambdaComp.CBPV.BindingConversion
import LambdaComp.CBPV.SkipReturn
import LambdaComp.CBPV.Syntax

topOptimizeDefault :: Tm 'Com -> Tm 'Com
topOptimizeDefault = topLiftingLet . topSkipReturn . topCommutingThen
