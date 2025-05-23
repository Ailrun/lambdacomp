module LambdaComp.CBPV.Optimization.BetaReduction
  ( runBetaReduction
  ) where

import LambdaComp.CBPV.Syntax

runBetaReduction :: Tm Com -> Tm Com
runBetaReduction = betaReduction

betaReduction :: Tm c -> Tm c
betaReduction tm@(TmVar _)             = tm
betaReduction tm@(TmGlobal _)          = tm
betaReduction tm@TmUnit                = tm
betaReduction tm@TmTrue                = tm
betaReduction tm@TmFalse               = tm
betaReduction tm@(TmInt _)             = tm
betaReduction tm@(TmDouble _)          = tm
betaReduction (TmThunk tm)             = TmThunk $ betaReduction tm
betaReduction (TmIf tm0 tm1 tm2)       =
  case betaReduction tm0 of
    TmTrue  -> tm1'
    TmFalse -> tm2'
    tm0'    -> TmIf tm0' tm1' tm2'
  where
    tm1' = betaReduction tm1
    tm2' = betaReduction tm2
betaReduction (TmLam p tm)             = TmLam p $ betaReduction tm
betaReduction (tmf `TmApp` tma)        =
  case betaReduction tmf of
    TmLam Param{..} tmf' -> TmLet paramName (betaReduction tma) tmf'
    tmf'                 -> tmf' `TmApp` betaReduction tma
betaReduction (TmForce tm)             =
  case betaReduction tm of
    TmThunk tm' -> tm'
    tm'         -> TmForce tm'
betaReduction (TmReturn tm)            = TmReturn $ betaReduction tm
betaReduction (TmTo tm0 x tm1)         =
  case betaReduction tm0 of
    TmReturn tm0' -> TmLet x tm0' (betaReduction tm1)
    tm0'          -> TmTo tm0' x (betaReduction tm1)
betaReduction (TmLet x tm0 tm1)        = TmLet x (betaReduction tm0) (betaReduction tm1)
betaReduction (TmPrimBinOp op tm0 tm1) = TmPrimBinOp op (betaReduction tm0) (betaReduction tm1)
betaReduction (TmPrimUnOp op tm)       = TmPrimUnOp op $ betaReduction tm
betaReduction (TmPrintInt tm0 tm1)     = TmPrintInt (betaReduction tm0) (betaReduction tm1)
betaReduction (TmPrintDouble tm0 tm1)  = TmPrintDouble (betaReduction tm0) (betaReduction tm1)
betaReduction (TmRec p tm)             = TmRec p (betaReduction tm)
