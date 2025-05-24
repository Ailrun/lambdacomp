module LambdaComp.CBPV.Optimization.EtaReduction
  ( runEtaReduction
  ) where

import LambdaComp.CBPV.Syntax

runEtaReduction :: Tm Com -> Tm Com
runEtaReduction = etaReduction

etaReduction :: Tm c -> Tm c
etaReduction tm@(TmVar _)                   = tm
etaReduction tm@(TmGlobal _)                = tm
etaReduction tm@TmUnit                      = tm
etaReduction tm@TmTrue                      = tm
etaReduction tm@TmFalse                     = tm
etaReduction tm@(TmInt _)                   = tm
etaReduction tm@(TmDouble _)                = tm
etaReduction (TmThunk (TmForce tm))         = etaReduction tm
etaReduction (TmThunk tm)                   = TmThunk $ etaReduction tm
etaReduction (TmIf tm0 tm1 tm2)             = TmIf (etaReduction tm0) (etaReduction tm1) (etaReduction tm2)
etaReduction (TmLam p (tm `TmApp` TmVar x))
  | paramName p == x                        = etaReduction tm
etaReduction (TmLam p tm)                   = TmLam p $ etaReduction tm
etaReduction (tmf `TmApp` tma)              = etaReduction tmf `TmApp` etaReduction tma
etaReduction (TmForce tm)                   = TmForce $ etaReduction tm
etaReduction (TmReturn tm)                  = TmReturn $ etaReduction tm
etaReduction (TmTo tm0 x tm1)               = TmTo (etaReduction tm0) x (etaReduction tm1)
etaReduction (TmLet x tm0 tm1)              = TmLet x (etaReduction tm0) (etaReduction tm1)
etaReduction (TmPrimBinOp op tm0 tm1)       = TmPrimBinOp op (etaReduction tm0) (etaReduction tm1)
etaReduction (TmPrimUnOp op tm)             = TmPrimUnOp op $ etaReduction tm
etaReduction (TmPrintInt tm0 tm1)           = TmPrintInt (etaReduction tm0) (etaReduction tm1)
etaReduction (TmPrintDouble tm0 tm1)        = TmPrintDouble (etaReduction tm0) (etaReduction tm1)
etaReduction (TmRec p tm)                   = TmRec p (etaReduction tm)
