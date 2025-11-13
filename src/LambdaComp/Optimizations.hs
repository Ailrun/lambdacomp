module LambdaComp.Optimizations
  ( module LambdaComp.Optimizations
  ) where
import Data.Set (Set)
import Data.Set qualified as Set

data Optimization where
  OElabConstantPropagation :: Optimization

  OCBPVBetaReduction,
    OCBPVCommutingIf,
    OCBPVCommutingPrint,
    OCBPVCommutingTo,
    OCBPVDeadLetElimination,
    OCBPVEtaReduction,
    OCBPVInlineSimpleLet,
    OCBPVInlineLinearLet,
    OCBPVLiftingLet :: Optimization
  deriving stock (Eq, Ord, Enum, Bounded, Show)

o0Optimization :: Set Optimization
o0Optimization = Set.empty

o1Optimization :: Set Optimization
o1Optimization = OCBPVCommutingIf `Set.delete` o2Optimization

o2Optimization :: Set Optimization
o2Optimization = Set.fromList [minBound..]

defaultOptimizations :: Set Optimization
defaultOptimizations = o1Optimization

integerToOptimizationClass :: Integer -> Set Optimization
integerToOptimizationClass 0 = o0Optimization
integerToOptimizationClass 1 = o1Optimization
integerToOptimizationClass n
  | n >= 2                   = o2Optimization
  | otherwise                = defaultOptimizations
