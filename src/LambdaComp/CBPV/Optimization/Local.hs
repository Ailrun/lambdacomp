module LambdaComp.CBPV.Optimization.Local
  ( runLocalOpt
  ) where

import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Set               (Set)
import Data.Set               qualified as Set

import LambdaComp.CBPV.Optimization.BetaReduction          (runBetaReduction)
import LambdaComp.CBPV.Optimization.BindingConversion      (runCommutingTo, runLiftingLet)
import LambdaComp.CBPV.Optimization.DeadBindingElimination (runDeadLetElimination)
import LambdaComp.CBPV.Optimization.EtaReduction           (runEtaReduction)
import LambdaComp.CBPV.Optimization.IfConversion           (runCommutingIf)
import LambdaComp.CBPV.Optimization.InlineBinding          (runInlineLinearLet, runInlineSimpleLet)
import LambdaComp.CBPV.Optimization.PrintConversion        (runCommutingPrint)
import LambdaComp.CBPV.PrettyPrinter                       ()
import LambdaComp.CBPV.Syntax
import LambdaComp.Optimizations                            (Optimization (..))

runLocalOpt :: Set Optimization -> Program -> Program
runLocalOpt opts = fmap (runLocalOptTop opts)

runLocalOptTop :: Set Optimization -> Top -> Top
runLocalOptTop opts = \m -> m{ tmDefBody = runLocalOptTm' $ tmDefBody m }
  where
    runLocalOptTm' = runLocalOptTm opts

runLocalOptTm :: Set Optimization -> Tm Com -> Tm Com
runLocalOptTm opts =
  runIdentity
  . repeatUntilFix
    (Identity
     . runDeadLetElimination'
     . runInlineLinearLet'
     . runInlineSimpleLet'
     . runLiftingLet'
     . runEtaReduction'
     . runBetaReduction'
     . runCommutingTo'
     . runCommutingPrint'
     . runCommutingIf')
  where
    runDeadLetElimination' = optionalByOption OCBPVDeadLetElimination runDeadLetElimination
    runInlineLinearLet' = optionalByOption OCBPVInlineLinearLet (runInlineLinearLet :: Tm Com -> Tm Com)
    runInlineSimpleLet' = optionalByOption OCBPVInlineSimpleLet (runInlineSimpleLet :: Tm Com -> Tm Com)
    runLiftingLet' = optionalByOption OCBPVLiftingLet runLiftingLet
    runEtaReduction' = optionalByOption OCBPVEtaReduction runEtaReduction
    runBetaReduction' = optionalByOption OCBPVBetaReduction runBetaReduction
    runCommutingTo' = optionalByOption OCBPVCommutingTo runCommutingTo
    runCommutingPrint' = optionalByOption OCBPVCommutingPrint runCommutingPrint
    runCommutingIf' = optionalByOption OCBPVCommutingIf runCommutingIf

    optionalByOption :: Optimization -> (a -> a) -> a -> a
    optionalByOption opt f
      | opt `Set.member` opts = f
      | otherwise             = id

repeatUntilFix :: (Monad m, Eq a) => (a -> m a) -> a -> m a
repeatUntilFix f a = do
  a' <- f a
  if a == a'
    then pure a'
    else repeatUntilFix f a'
