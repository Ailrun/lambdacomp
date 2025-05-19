{-# LANGUAGE RecordWildCards #-}
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
runLocalOptDefaultTop TopTmDef {..} = TopTmDef { tmDefName, tmDefBody = runLocalOptDefaultTm tmDefBody }

runLocalOptDefaultTm :: Tm Val -> Tm Val
runLocalOptDefaultTm = runDeadLetElimination . runInlineSimpleLet . runLiftingLet . runSkipReturn . runCommutingTo
