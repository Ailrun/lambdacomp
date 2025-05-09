{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.Elaborated.CBV.ToCBPV where

import Control.Monad.State.Strict (evalState)
import Data.Functor.Identity      (Identity (..))
import Data.Functor.Product       (Product (..))
import Data.List                  (foldl')
import Data.String                (IsString (fromString))

import LambdaComp.CBPV.Syntax       qualified as CBPV
import LambdaComp.Elaborated.Syntax
import LambdaComp.FreshName

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
    tmDefBody' <- toCBPV tmDefBody
    let tmDefBody'' =
          case tmDefBody' of
            CBPV.TmReturn tm -> tm
            _                -> CBPV.TmThunk tmDefBody'
    pure $ CBPV.TopTmDef { tmDefName = "u_" <> tmDefName, tmDefBody = tmDefBody'' }

instance ToCBPV Tp where
  type CBPVData Tp = CBPV.Tp CBPV.Val

  toCBPV :: Tp -> CBPVData Tp
  toCBPV = helper
    where
      helper :: Tp -> CBPV.Tp CBPV.Val
      helper TpUnit             = CBPV.TpUnit
      helper TpBool             = CBPV.TpBool
      helper TpInt              = CBPV.TpInt
      helper TpDouble           = CBPV.TpDouble
      helper (tyPs `TpFun` tyR) = foldr ((CBPV.TpUp .) . (. CBPV.TpDown) . CBPV.TpFun . helper) (helper tyR) tyPs

instance ToCBPV Tm where
  type CBPVData Tm = FreshName (CBPV.Tm CBPV.Com)

  toCBPV :: Tm -> CBPVData Tm
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
  toCBPV (TmLam ps tm)            = foldr (((CBPV.TmReturn . CBPV.TmThunk) .) . CBPV.TmLam . toCBPV) <$> toCBPV tm <*> pure ps
  toCBPV (tmf `TmApp` tmas)       = do
    tmas' <- traverse toCBPV tmas
    tmf' <- toCBPV tmf
    Pair as fs <- freshNamesOf (Pair (fmap (("c_a" <>) . fromString . show) [0..(length tmas - 1)]) (fmap (("c_f" <>) . fromString . show) [0..(length tmas - 1)]))
    pure $ foldl' (.) id (zipWith CBPV.TmTo tmas' as) $ foldl' (uncurry . appTmOnVar) tmf' $ zip fs as
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
  toCBPV (TmPrintDouble tm0 tm1)  = do
    tm0' <- toCBPV tm0
    v <- freshNameOf "c_v"
    CBPV.TmTo tm0' v . CBPV.TmPrintDouble (CBPV.TmVar v) <$> toCBPV tm1
  toCBPV (TmRec x tp tm)          = do
    tm' <- toCBPV tm
    v <- freshNameOf "c_r"
    pure $ CBPV.TmReturn . CBPV.TmThunk . CBPV.TmRec ("u_" <> x) (toCBPV tp) $ CBPV.TmTo tm' v $ CBPV.TmForce (CBPV.TmVar v)

instance ToCBPV Param where
  type CBPVData Param = CBPV.Param

  toCBPV :: Param -> CBPVData Param
  toCBPV Param {..} = CBPV.Param ("u_" <> paramName) (toCBPV paramType)

appTmOnVar :: CBPV.Tm CBPV.Com -> Ident -> Ident -> CBPV.Tm CBPV.Com
appTmOnVar tmf f a = CBPV.TmTo tmf f $ CBPV.TmForce (CBPV.TmVar f) `CBPV.TmApp` CBPV.TmVar a
