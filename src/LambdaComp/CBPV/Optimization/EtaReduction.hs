module LambdaComp.CBPV.Optimization.EtaReduction
  ( runEtaReduction
  ) where

import Data.Set qualified as Set

import LambdaComp.Binder      (mapBinderBody)
import LambdaComp.CBPV.Syntax

runEtaReduction :: Tm Com -> Tm Com
runEtaReduction = etaReduction

etaReduction :: Tm c -> Tm c

etaReduction tm@(TmVar _
                ; TmGlobal _
                ; TmConst _)        = polyRecTm etaReduction tm
etaReduction (TmThunk (TmForce tm)) = etaReduction tm
etaReduction (TmThunk tm)           = TmThunk $ etaReduction tm

etaReduction tm@(TmIf {}
                ; TmApp {}
                ; TmForce _
                ; TmReturn _; TmTo {}
                ; TmLet {}
                ; TmPrimBinOp {}; TmPrimUnOp {}
                ; TmPrintInt {}; TmPrintDouble {}
                ; TmRec _)           = polyRecTm etaReduction tm
etaReduction (TmLam (BTyped p (tm `TmApp` TmVar x)))
  | paramName p == x
  , x `Set.notMember` freeVarOfTm tm = etaReduction tm
etaReduction (TmLam b)               = TmLam $ mapBinderBody etaReduction b
