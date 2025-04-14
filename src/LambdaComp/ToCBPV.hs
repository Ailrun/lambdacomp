{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.ToCBPV where

import Control.Monad.State.Strict (MonadState (get), State, modify', evalState)
import Data.String                (IsString (fromString))

import LambdaComp.CBPV.Syntax     qualified as CBPV
import LambdaComp.Syntax

type FreshName = State Integer

class ToCBPV a where
  type CBPVData a
  toCBPV :: a -> FreshName (CBPVData a)

instance ToCBPV Tp where
  type CBPVData Tp = CBPV.Tp 'CBPV.Com

  toCBPV :: Tp -> FreshName (CBPVData Tp)
  toCBPV = pure . CBPV.TpDown . helper
    where
      helper :: Tp -> CBPV.Tp 'CBPV.Val
      helper TpUnit            = CBPV.TpUnit
      helper TpInt             = CBPV.TpInt
      helper TpDouble          = CBPV.TpDouble
      helper (ty0 `TpFun` ty1) = CBPV.TpUp $ helper ty0 `CBPV.TpFun` CBPV.TpDown (helper ty1)

instance ToCBPV Tm where
  type CBPVData Tm = CBPV.Tm 'CBPV.Com

  toCBPV :: Tm -> FreshName (CBPVData Tm)
  toCBPV (tm `TmAnn` _)    = toCBPV tm
  toCBPV (TmVar x)         = pure $ CBPV.TmReturn $ CBPV.TmVar x
  toCBPV TmUnit            = pure $ CBPV.TmReturn CBPV.TmUnit
  toCBPV (TmInt n)         = pure $ CBPV.TmReturn $ CBPV.TmInt n
  toCBPV (TmDouble f)      = pure $ CBPV.TmReturn $ CBPV.TmDouble f
  toCBPV (TmLam x tm)      = CBPV.TmReturn . CBPV.TmThunk . CBPV.TmLam x <$> toCBPV tm
  toCBPV (tmf `TmApp` tma) = do
    tma' <- toCBPV tma
    i <- get
    modify' (1 +)
    let ai = "a___" <> fromString (show i)

    tmf' <- toCBPV tmf
    j <- get
    modify' (1 +)
    let fj = "f___" <> fromString (show j)

    pure $ CBPV.TmThen tma' ai $ CBPV.TmThen tmf' fj $ CBPV.TmForce (CBPV.TmVar fj) `CBPV.TmApp` CBPV.TmVar ai
  toCBPV (TmPrint tm0 tm1) = do
    tm0' <- toCBPV tm0
    i <- get
    modify' (1 +)
    let vi = "v___" <> fromString (show i)
    CBPV.TmThen tm0' vi . CBPV.TmPrint (CBPV.TmVar vi) <$> toCBPV tm1
  toCBPV (TmRec x tm)      = CBPV.TmRec x <$> toCBPV tm

runToCBPV :: Tm -> CBPV.Tm 'CBPV.Com
runToCBPV = (`evalState` 0) . toCBPV
