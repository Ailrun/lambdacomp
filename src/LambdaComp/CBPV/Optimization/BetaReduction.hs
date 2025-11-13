module LambdaComp.CBPV.Optimization.BetaReduction
  ( runBetaReduction
  ) where

import LambdaComp.Binder      (getBinderBody, getBoundVar, mapBinderBody)
import LambdaComp.CBPV.Syntax

runBetaReduction :: Tm Com -> Tm Com
runBetaReduction = betaReduction

betaReduction :: Tm c -> Tm c
betaReduction tm@(TmVar _
                 ; TmGlobal _
                 ; TmConst _
                 ; TmThunk _) = polyRecTm betaReduction tm

betaReduction tm@(TmLam _
                 ; TmReturn _
                 ; TmLet {}
                 ; TmPrimBinOp {}; TmPrimUnOp {}
                 ; TmPrintInt {}; TmPrintDouble {}
                 ; TmRec _)      = polyRecTm betaReduction tm
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
    TmLam b -> TmLet tma' . BUntyped (getBoundVar b) $ getBinderBody b
    tmf'    -> tmf' `TmApp` tma'
  where
    tma' = betaReduction tma
betaReduction (TmForce tm)       =
  case betaReduction tm of
    TmThunk tm' -> tm'
    tm'         -> TmForce tm'
betaReduction (TmTo tm0 b)       =
  case betaReduction tm0 of
    TmReturn tm0' -> TmLet tm0' b'
    tm0'          -> TmTo tm0' b'
  where
    b' = mapBinderBody betaReduction b
