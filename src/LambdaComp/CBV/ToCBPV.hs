{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.CBV.ToCBPV where

import Control.Monad.State.Strict (evalState)

import LambdaComp.CBPV.Syntax qualified as CBPV
import LambdaComp.FreshName
import LambdaComp.Syntax

runToCBPV :: Tm -> CBPV.Tm CBPV.Com
runToCBPV = (`evalState` 0) . toCBPV

class ToCBPV a where
  type CBPVData a
  toCBPV :: a -> FreshName (CBPVData a)

instance ToCBPV Tp where
  type CBPVData Tp = CBPV.Tp CBPV.Com

  toCBPV :: Tp -> FreshName (CBPVData Tp)
  toCBPV = pure . CBPV.TpDown . helper
    where
      helper :: Tp -> CBPV.Tp CBPV.Val
      helper TpUnit            = CBPV.TpUnit
      helper TpInt             = CBPV.TpInt
      helper TpDouble          = CBPV.TpDouble
      helper (ty0 `TpFun` ty1) = CBPV.TpUp $ helper ty0 `CBPV.TpFun` CBPV.TpDown (helper ty1)

instance ToCBPV Tm where
  type CBPVData Tm = CBPV.Tm CBPV.Com

  toCBPV :: Tm -> FreshName (CBPVData Tm)
  toCBPV (tm `TmAnn` _)       = toCBPV tm
  toCBPV (TmVar x)            = pure $ CBPV.TmReturn $ CBPV.TmVar $ "u_" <> x
  toCBPV TmUnit               = pure $ CBPV.TmReturn CBPV.TmUnit
  toCBPV (TmInt n)            = pure $ CBPV.TmReturn $ CBPV.TmInt n
  toCBPV (TmDouble f)         = pure $ CBPV.TmReturn $ CBPV.TmDouble f
  toCBPV (TmLam x tm)         = CBPV.TmReturn . CBPV.TmThunk . CBPV.TmLam ("u_" <> x) <$> toCBPV tm
  toCBPV (tmf `TmApp` tma)    = do
    tma' <- toCBPV tma
    a <- freshNameOf "c_a"

    tmf' <- toCBPV tmf
    f <- freshNameOf "c_f"

    pure $ CBPV.TmThen tma' a $ CBPV.TmThen tmf' f $ CBPV.TmForce (CBPV.TmVar f) `CBPV.TmApp` CBPV.TmVar a
  toCBPV (TmPrintInt tm0 tm1) = do
    tm0' <- toCBPV tm0
    v <- freshNameOf "c_v"
    CBPV.TmThen tm0' v . CBPV.TmPrintInt (CBPV.TmVar v) <$> toCBPV tm1
  toCBPV (TmRec x tm)         = do
    tm' <- toCBPV tm
    v <- freshNameOf "c_r"
    pure $ CBPV.TmReturn . CBPV.TmThunk . CBPV.TmRec ("u_" <> x) $ CBPV.TmThen tm' v $ CBPV.TmForce (CBPV.TmVar v)
