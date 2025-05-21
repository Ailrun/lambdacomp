{-# LANGUAGE TypeFamilies #-}
module LambdaComp.Elaborated.CBV.ToCBPV
  ( runToCBPV
  ) where

import Control.Monad.FreshName (FreshName, freshNameOf, freshNamesOf, runFreshName)
import Data.Functor.Identity   (Identity (..))
import Data.Functor.Product    (Product (..))
import Data.List               (foldl')
import Data.String             (IsString (fromString))

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
  toCBPV TopTmDef {..} = CBPV.TopTmDef (toUserVar tmDefName) <$> toCBPV tmDefBody

instance ToCBPV Tp where
  type CBPVData Tp = CBPV.Tp CBPV.Val

  toCBPV :: Tp -> CBPVData Tp
  toCBPV TpUnit             = CBPV.TpUnit
  toCBPV TpBool             = CBPV.TpBool
  toCBPV TpInt              = CBPV.TpInt
  toCBPV TpDouble           = CBPV.TpDouble
  toCBPV (tyPs `TpFun` tyR) = foldr ((CBPV.TpUp .) . (. CBPV.TpDown) . CBPV.TpFun . toCBPV) (toCBPV tyR) tyPs

instance ToCBPV Tm where
  type CBPVData Tm = FreshName (CBPV.Tm CBPV.Com)

  toCBPV :: Tm -> CBPVData Tm
  toCBPV (TmVar x)                = pure $ CBPV.TmReturn $ CBPV.TmVar $ toUserVar x
  toCBPV (TmGlobal x)             = pure $ CBPV.TmReturn $ CBPV.TmGlobal $ toUserVar x
  toCBPV TmUnit                   = pure $ CBPV.TmReturn CBPV.TmUnit
  toCBPV TmTrue                   = pure $ CBPV.TmReturn CBPV.TmTrue
  toCBPV TmFalse                  = pure $ CBPV.TmReturn CBPV.TmFalse
  toCBPV (TmInt n)                = pure $ CBPV.TmReturn $ CBPV.TmInt n
  toCBPV (TmDouble f)             = pure $ CBPV.TmReturn $ CBPV.TmDouble f
  toCBPV (TmIf tm0 tm1 tm2)       = do
    tm0' <- toCBPV tm0
    c <- freshNameOf $ toCBPVVar "c"
    CBPV.TmTo tm0' c <$> liftA2 (CBPV.TmIf (CBPV.TmVar c)) (toCBPV tm1) (toCBPV tm2)
  toCBPV (TmLam ps tm)            = foldr (((CBPV.TmReturn . CBPV.TmThunk) .) . CBPV.TmLam . toCBPV) <$> toCBPV tm <*> pure ps
  toCBPV (tmf `TmApp` tmas)       = do
    tmas' <- traverse toCBPV tmas
    tmf' <- toCBPV tmf
    Pair as fs <- freshNamesOf (Pair (fmap ((toCBPVVar "a" <>) . fromString . show) [0..(length tmas - 1)]) (fmap ((toCBPVVar "f" <>) . fromString . show) [0..(length tmas - 1)]))
    pure $ foldl' (.) id (zipWith CBPV.TmTo tmas' as) $ foldl' (uncurry . appTmOnVar) tmf' $ zip fs as
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
  toCBPV (TmRec (Param {..}) tm)  = do
    tm' <- toCBPV tm
    v <- freshNameOf $ toCBPVVar "r"
    pure $ CBPV.TmReturn . CBPV.TmThunk . CBPV.TmRec (CBPV.Param (toUserVar paramName) (toCBPV paramType)) $ CBPV.TmTo tm' v $ CBPV.TmForce (CBPV.TmVar v)

instance ToCBPV Param where
  type CBPVData Param = CBPV.Param

  toCBPV :: Param -> CBPVData Param
  toCBPV Param {..} = CBPV.Param (toUserVar paramName) (toCBPV paramType)

appTmOnVar :: CBPV.Tm CBPV.Com -> Ident -> Ident -> CBPV.Tm CBPV.Com
appTmOnVar tmf f a = CBPV.TmTo tmf f $ CBPV.TmForce (CBPV.TmVar f) `CBPV.TmApp` CBPV.TmVar a
