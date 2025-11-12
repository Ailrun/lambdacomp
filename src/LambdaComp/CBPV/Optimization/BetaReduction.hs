module LambdaComp.CBPV.Optimization.BetaReduction
  ( runBetaReduction
  ) where

import Data.Functor.Identity (Identity (Identity, runIdentity))

import LambdaComp.CBPV.Syntax

runBetaReduction :: Tm Com -> Tm Com
runBetaReduction = betaReduction

betaReduction :: Tm c -> Tm c
betaReduction tm@(TmVar _
                 ; TmGlobal _
                 ; TmConst _
                 ; TmThunk _) = runIdentity $ polyRecTmM (Identity . betaReduction) tm

betaReduction tm@(TmLam {}
                 ; TmReturn _
                 ; TmLet {}
                 ; TmPrimBinOp {}; TmPrimUnOp {}
                 ; TmPrintInt {}; TmPrintDouble {}
                 ; TmRec {})     = runIdentity $ polyRecTmM (Identity . betaReduction) tm
betaReduction (TmIf tm0 tm1 tm2) =
  case betaReduction tm0 of
    TmConst TmCTrue  -> tm1'
    TmConst TmCFalse -> tm2'
    tm0'             -> TmIf tm0' tm1' tm2'
  where
    tm1' = betaReduction tm1
    tm2' = betaReduction tm2
betaReduction (tmf `TmApp` tma)  =
  case betaReduction tmf of
    TmLam Param{..} tmf' -> TmLet paramName tma' tmf'
    tmf'                 -> tmf' `TmApp` tma'
  where
    tma' = betaReduction tma
betaReduction (TmForce tm)       =
  case betaReduction tm of
    TmThunk tm' -> tm'
    tm'         -> TmForce tm'
betaReduction (TmTo tm0 x tm1)   =
  case betaReduction tm0 of
    TmReturn tm0' -> TmLet x tm0' tm1'
    tm0'          -> TmTo tm0' x tm1'
  where
    tm1' = betaReduction tm1
