{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.CBV.ToCBPV where

import Control.Monad.State.Strict (evalState)
import Data.Functor.Identity      (Identity (..))
import Data.Functor.Product       (Product (..))

import LambdaComp.CBPV.Syntax qualified as CBPV
import LambdaComp.FreshName
import LambdaComp.Syntax

runToCBPV :: Program -> CBPV.Program
runToCBPV = (`evalState` 0) . toCBPV

class ToCBPV a where
  type CBPVData a
  toCBPV :: a -> CBPVData a

instance ToCBPV Program where
  type CBPVData Program = FreshName CBPV.Program

  toCBPV :: Program -> CBPVData Program
  toCBPV = traverse toCBPV

instance ToCBPV Top where
  type CBPVData Top = FreshName CBPV.Top

  toCBPV :: Top -> CBPVData Top
  toCBPV TopTmDef {..} = do
    let tmDefType' = toCBPV tmDefType
    tmDefBody' <- toCBPV tmDefBody
    let (tmDefType'', tmDefBody'') =
          case (tmDefType', tmDefBody') of
            (CBPV.TpDown tp, CBPV.TmReturn tm) -> (tp, tm)
            _ -> (CBPV.TpUp tmDefType', CBPV.TmThunk tmDefBody')
    pure $ CBPV.TopTmDef { tmDefName = "u_" <> tmDefName, tmDefType = tmDefType'', tmDefBody = tmDefBody'' }

instance ToCBPV Tp where
  type CBPVData Tp = CBPV.Tp CBPV.Com

  toCBPV :: Tp -> CBPVData Tp
  toCBPV = CBPV.TpDown . helper
    where
      helper :: Tp -> CBPV.Tp CBPV.Val
      helper TpUnit            = CBPV.TpUnit
      helper TpBool            = CBPV.TpBool
      helper TpInt             = CBPV.TpInt
      helper TpDouble          = CBPV.TpDouble
      helper (ty0 `TpFun` ty1) = CBPV.TpUp $ helper ty0 `CBPV.TpFun` CBPV.TpDown (helper ty1)

instance ToCBPV Tm where
  type CBPVData Tm = FreshName (CBPV.Tm CBPV.Com)

  toCBPV :: Tm -> CBPVData Tm
  toCBPV (tm `TmAnn` _)           = toCBPV tm
  toCBPV (TmVar x)                = pure $ CBPV.TmReturn $ CBPV.TmVar $ "u_" <> x
  toCBPV TmUnit                   = pure $ CBPV.TmReturn CBPV.TmUnit
  toCBPV TmTrue                   = pure $ CBPV.TmReturn CBPV.TmTrue
  toCBPV TmFalse                  = pure $ CBPV.TmReturn CBPV.TmFalse
  toCBPV (TmInt n)                = pure $ CBPV.TmReturn $ CBPV.TmInt n
  toCBPV (TmDouble f)             = pure $ CBPV.TmReturn $ CBPV.TmDouble f
  toCBPV (TmIf tm0 tm1 tm2)       = do
    tm0' <- toCBPV tm0
    c <- freshNameOf "c_c"
    CBPV.TmTo tm0' c <$> liftA2 (CBPV.TmIf (CBPV.TmVar c)) (toCBPV tm1) (toCBPV tm2)
  toCBPV (TmLam x tm)             = CBPV.TmReturn . CBPV.TmThunk . CBPV.TmLam ("u_" <> x) <$> toCBPV tm
  toCBPV (tmf `TmApp` tma)        = do
    tma' <- toCBPV tma
    tmf' <- toCBPV tmf
    Pair (Identity a) (Identity f) <- freshNamesOf (Pair "c_a" "c_f")
    pure $ CBPV.TmTo tma' a $ CBPV.TmTo tmf' f $ CBPV.TmForce (CBPV.TmVar f) `CBPV.TmApp` CBPV.TmVar a
  toCBPV (TmPrimBinOp op tm0 tm1) = do
    tm0' <- toCBPV tm0
    tm1' <- toCBPV tm1
    Pair (Identity inp0) (Identity inp1) <- freshNamesOf (Pair "c_inp0" "c_inp1")
    pure $ CBPV.TmTo tm0' inp0 $ CBPV.TmTo tm1' inp1 $ CBPV.TmPrimBinOp op (CBPV.TmVar inp0) (CBPV.TmVar inp1)
  toCBPV (TmPrimUnOp op tm)       = do
    tm' <- toCBPV tm
    inp <- freshNameOf "c_inp"
    pure $ CBPV.TmTo tm' inp $ CBPV.TmPrimUnOp op (CBPV.TmVar inp)
  toCBPV (TmPrintInt tm0 tm1)     = do
    tm0' <- toCBPV tm0
    v <- freshNameOf "c_v"
    CBPV.TmTo tm0' v . CBPV.TmPrintInt (CBPV.TmVar v) <$> toCBPV tm1
  toCBPV (TmRec x tm)             = do
    tm' <- toCBPV tm
    v <- freshNameOf "c_r"
    pure $ CBPV.TmReturn . CBPV.TmThunk . CBPV.TmRec ("u_" <> x) $ CBPV.TmTo tm' v $ CBPV.TmForce (CBPV.TmVar v)
