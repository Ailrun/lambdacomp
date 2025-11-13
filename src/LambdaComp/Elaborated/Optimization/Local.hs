module LambdaComp.Elaborated.Optimization.Local
  ( runLocalOpt
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

import LambdaComp.Elaborated.Optimization.ConstantPropagation
import LambdaComp.Elaborated.Syntax
import LambdaComp.Optimizations                               (Optimization (..))

runLocalOpt :: Set Optimization -> Program -> Program
runLocalOpt opt = fmap (runLocalOptTop opt)

runLocalOptTop :: Set Optimization -> Top -> Top
runLocalOptTop opt = \m -> m{ tmDefBody = runLocalOptTm' $ tmDefBody m }
  where
    runLocalOptTm' = runLocalOptTm opt

runLocalOptTm :: Set Optimization -> Tm -> Tm
runLocalOptTm opt = runConstantsPropagation'
  where
    runConstantsPropagation'
      | OElabConstantPropagation `Set.member` opt = runConstantsPropagation
      | otherwise                                 = id
