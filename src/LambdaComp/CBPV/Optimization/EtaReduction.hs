module LambdaComp.CBPV.Optimization.EtaReduction
  ( runEtaReduction
  ) where

import Control.Monad.Identity (Identity (Identity, runIdentity))

import LambdaComp.CBPV.Syntax

runEtaReduction :: Tm Com -> Tm Com
runEtaReduction = etaReduction

etaReduction :: Tm c -> Tm c

etaReduction tm@(TmVar _
                ; TmGlobal _
                ; TmConst _)        = runIdentity $ polyRecTmM (Identity . etaReduction) tm
etaReduction (TmThunk (TmForce tm)) = etaReduction tm
etaReduction (TmThunk tm)           = TmThunk $ etaReduction tm

etaReduction tm@(TmIf {}
                ; TmApp {}
                ; TmForce _
                ; TmReturn _; TmTo {}
                ; TmLet {}
                ; TmPrimBinOp {}; TmPrimUnOp {}
                ; TmPrintInt {}; TmPrintDouble {}
                ; TmRec {})                 = runIdentity $ polyRecTmM (Identity . etaReduction) tm
etaReduction (TmLam p (tm `TmApp` TmVar x))
  | paramName p == x                        = etaReduction tm
etaReduction (TmLam p tm)                   = TmLam p $ etaReduction tm
