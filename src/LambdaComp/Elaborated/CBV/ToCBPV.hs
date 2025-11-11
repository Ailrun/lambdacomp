{-# LANGUAGE TypeFamilies #-}
module LambdaComp.Elaborated.CBV.ToCBPV
  ( runToCBPV
  ) where

import Control.Monad.FreshName (FreshName, freshNameOf, freshNamesOf, runFreshName)
import Data.Functor.Identity   (Identity (..))
import Data.Functor.Product    (Product (..))

import LambdaComp.CBPV.Syntax       qualified as CBPV
import LambdaComp.Elaborated.Syntax

runToCBPV :: Program -> CBPV.Program
runToCBPV = runFreshName . toCBPV

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
  toCBPV TopTmDef {..} = CBPV.TopTmDef tmDefName <$> toCBPV tmDefBody

instance ToCBPV Tp where
  type CBPVData Tp = CBPV.Tp CBPV.Val

  toCBPV :: Tp -> CBPVData Tp
  toCBPV (TpConst tpc)     = CBPV.TpConst $ toCBPV tpc
  toCBPV (tyP `TpFun` tyR) = CBPV.TpUp $ toCBPV tyP `CBPV.TpFun` CBPV.TpDown (toCBPV tyR)

instance ToCBPV TpConst where
  type CBPVData TpConst = CBPV.TpConst

  toCBPV :: TpConst -> CBPVData TpConst
  toCBPV TpCUnit   = CBPV.TpCUnit
  toCBPV TpCBool   = CBPV.TpCBool
  toCBPV TpCInt    = CBPV.TpCInt
  toCBPV TpCDouble = CBPV.TpCDouble

instance ToCBPV Tm where
  type CBPVData Tm = FreshName (CBPV.Tm CBPV.Com)

  toCBPV :: Tm -> CBPVData Tm
  toCBPV (TmVar x)                = pure $ CBPV.TmReturn $ CBPV.TmVar x
  toCBPV (TmGlobal x)             = pure $ CBPV.TmReturn $ CBPV.TmGlobal x
  toCBPV (TmConst c)              = pure . CBPV.TmReturn . CBPV.TmConst $ toCBPV c
  toCBPV (TmIf tm0 tm1 tm2)       = do
    tm0' <- toCBPV tm0
    c <- freshNameOf $ toCBPVVar "c"
    CBPV.TmTo tm0' c <$> liftA2 (CBPV.TmIf (CBPV.TmVar c)) (toCBPV tm1) (toCBPV tm2)
  toCBPV (TmLam p tm)             = CBPV.TmReturn . CBPV.TmThunk . CBPV.TmLam (toCBPV p) <$> toCBPV tm
  toCBPV (tmf `TmApp` tma)        = do
    tma' <- toCBPV tma
    tmf' <- toCBPV tmf
    Pair (Identity a) (Identity f) <- freshNamesOf (Pair (toCBPVVar <$> "a") (toCBPVVar <$> "f"))
    pure $ CBPV.TmTo tma' a (appTmOnVar tmf' f a)
  toCBPV (TmPrimBinOp op tm0 tm1) = do
    tm0' <- toCBPV tm0
    tm1' <- toCBPV tm1
    Pair (Identity inp0) (Identity inp1) <- freshNamesOf (Pair (toCBPVVar <$> "inp0") (toCBPVVar <$> "inp1"))
    pure $ CBPV.TmTo tm0' inp0 $ CBPV.TmTo tm1' inp1 $ CBPV.TmPrimBinOp op (CBPV.TmVar inp0) (CBPV.TmVar inp1)
  toCBPV (TmPrimUnOp op tm)       = do
    tm' <- toCBPV tm
    inp <- freshNameOf $ toCBPVVar "inp"
    pure $ CBPV.TmTo tm' inp $ CBPV.TmPrimUnOp op (CBPV.TmVar inp)
  toCBPV (TmPrintInt tm0 tm1)     = do
    tm0' <- toCBPV tm0
    v <- freshNameOf $ toCBPVVar "v"
    CBPV.TmTo tm0' v . CBPV.TmPrintInt (CBPV.TmVar v) <$> toCBPV tm1
  toCBPV (TmPrintDouble tm0 tm1)  = do
    tm0' <- toCBPV tm0
    v <- freshNameOf $ toCBPVVar "v"
    CBPV.TmTo tm0' v . CBPV.TmPrintDouble (CBPV.TmVar v) <$> toCBPV tm1
  toCBPV (TmRec p tm)             = do
    tm' <- toCBPV tm
    v <- freshNameOf $ toCBPVVar "r"
    pure $ CBPV.TmReturn . CBPV.TmThunk . CBPV.TmRec (toCBPV p) $ CBPV.TmTo tm' v $ CBPV.TmForce (CBPV.TmVar v)

instance ToCBPV TmConst where
  type CBPVData TmConst = CBPV.TmConst

  toCBPV :: TmConst -> CBPVData TmConst
  toCBPV TmCUnit                   = CBPV.TmCUnit
  toCBPV TmCTrue                   = CBPV.TmCTrue
  toCBPV TmCFalse                  = CBPV.TmCFalse
  toCBPV (TmCInt n)                = CBPV.TmCInt n
  toCBPV (TmCDouble f)             = CBPV.TmCDouble f

instance ToCBPV Param where
  type CBPVData Param = CBPV.Param

  toCBPV :: Param -> CBPVData Param
  toCBPV Param {..} = CBPV.Param paramName (toCBPV paramType)

appTmOnVar :: CBPV.Tm CBPV.Com -> Ident -> Ident -> CBPV.Tm CBPV.Com
appTmOnVar tmf f a = CBPV.TmTo tmf f $ CBPV.TmForce (CBPV.TmVar f) `CBPV.TmApp` CBPV.TmVar a
