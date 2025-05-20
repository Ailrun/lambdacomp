module LambdaComp.Elaborated.Optimization.ConstantPropagation
  ( runConstantsPropagation
  ) where

import Data.Maybe (fromMaybe)

import LambdaComp.Elaborated.Syntax

runConstantsPropagation :: Tm -> Tm
runConstantsPropagation = propagateConstants

propagateConstants :: Tm -> Tm
propagateConstants tm@(TmVar _)             = tm
propagateConstants tm@(TmGlobal _)          = tm
propagateConstants tm@TmUnit                = tm
propagateConstants tm@TmTrue                = tm
propagateConstants tm@TmFalse               = tm
propagateConstants tm@(TmInt _)             = tm
propagateConstants tm@(TmDouble _)          = tm
propagateConstants (TmIf tm0 tm1 tm2)       = TmIf (propagateConstants tm0) (propagateConstants tm1) (propagateConstants tm2)
propagateConstants (TmLam ps tm)            = TmLam ps $ propagateConstants tm
propagateConstants (tmf `TmApp` tma)        = propagateConstants tmf `TmApp` fmap propagateConstants tma
propagateConstants (TmPrimBinOp op tm0 tm1) = fromMaybe (TmPrimBinOp op tm0' tm1') $ propagateWithBinOp op tm0' tm1'
  where
    tm0' = propagateConstants tm0
    tm1' = propagateConstants tm1
propagateConstants (TmPrimUnOp op tm)       = fromMaybe (TmPrimUnOp op tm') $ propagateWithUnOp op tm
  where
    tm' = propagateConstants tm
propagateConstants (TmPrintInt tm0 tm1)     = TmPrintInt (propagateConstants tm0) (propagateConstants tm1)
propagateConstants (TmPrintDouble tm0 tm1)  = TmPrintDouble (propagateConstants tm0) (propagateConstants tm1)
propagateConstants (TmRec p tm)             = TmRec p (propagateConstants tm)

propagateWithBinOp :: PrimOp Binary -> Tm -> Tm -> Maybe Tm
propagateWithBinOp PrimIAdd = propagateWithBinIntOp (+) TmInt
propagateWithBinOp PrimISub = propagateWithBinIntOp (-) TmInt
propagateWithBinOp PrimIMul = propagateWithBinIntOp (*) TmInt
propagateWithBinOp PrimIDiv = propagateWithBinIntOp quot TmInt
propagateWithBinOp PrimIMod = propagateWithBinIntOp rem TmInt
propagateWithBinOp PrimIEq  = propagateWithBinIntOp (==) boolToTm
propagateWithBinOp PrimINEq = propagateWithBinIntOp (/=) boolToTm
propagateWithBinOp PrimILt  = propagateWithBinIntOp (<) boolToTm
propagateWithBinOp PrimILe  = propagateWithBinIntOp (<=) boolToTm
propagateWithBinOp PrimIGt  = propagateWithBinIntOp (>) boolToTm
propagateWithBinOp PrimIGe  = propagateWithBinIntOp (>=) boolToTm
propagateWithBinOp PrimDAdd = propagateWithBinDoubleOp (+) TmDouble
propagateWithBinOp PrimDSub = propagateWithBinDoubleOp (-) TmDouble
propagateWithBinOp PrimDMul = propagateWithBinDoubleOp (*) TmDouble
propagateWithBinOp PrimDDiv = propagateWithBinDoubleOp (/) TmDouble
propagateWithBinOp PrimDEq  = propagateWithBinDoubleOp (==) boolToTm
propagateWithBinOp PrimDNEq = propagateWithBinDoubleOp (/=) boolToTm
propagateWithBinOp PrimDLt  = propagateWithBinDoubleOp (<) boolToTm
propagateWithBinOp PrimDLe  = propagateWithBinDoubleOp (<=) boolToTm
propagateWithBinOp PrimDGt  = propagateWithBinDoubleOp (>) boolToTm
propagateWithBinOp PrimDGe  = propagateWithBinDoubleOp (>=) boolToTm
propagateWithBinOp PrimBAnd = propagateWithBinBoolOp (&&) boolToTm
propagateWithBinOp PrimBOr  = propagateWithBinBoolOp (||) boolToTm

propagateWithBinIntOp :: (Int -> Int -> a) -> (a -> Tm) -> Tm -> Tm -> Maybe Tm
propagateWithBinIntOp = propagateWithBinConstOp intTmToInt

propagateWithBinDoubleOp :: (Double -> Double -> a) -> (a -> Tm) -> Tm -> Tm -> Maybe Tm
propagateWithBinDoubleOp = propagateWithBinConstOp doubleTmToDouble

propagateWithBinBoolOp :: (Bool -> Bool -> a) -> (a -> Tm) -> Tm -> Tm -> Maybe Tm
propagateWithBinBoolOp = propagateWithBinConstOp boolTmToBool

propagateWithBinConstOp :: (Tm -> Maybe c) -> (c -> c -> a) -> (a -> Tm) -> Tm -> Tm -> Maybe Tm
propagateWithBinConstOp getC comp wrap tm0 tm1 = wrap <$> (comp <$> getC tm0 <*> getC tm1)

propagateWithUnOp :: PrimOp Unary -> Tm -> Maybe Tm
propagateWithUnOp PrimINeg = propagateWithUnIntOp negate TmInt
propagateWithUnOp PrimIToD = propagateWithUnIntOp fromIntegral TmDouble
propagateWithUnOp PrimDNeg = propagateWithUnDoubleOp negate TmDouble
propagateWithUnOp PrimDToI = propagateWithUnDoubleOp truncate TmInt
propagateWithUnOp PrimBNot = propagateWithUnBoolOp not boolToTm

propagateWithUnIntOp :: (Int -> a) -> (a -> Tm) -> Tm -> Maybe Tm
propagateWithUnIntOp = propagateWithUnConstOp intTmToInt

propagateWithUnDoubleOp :: (Double -> a) -> (a -> Tm) -> Tm -> Maybe Tm
propagateWithUnDoubleOp = propagateWithUnConstOp doubleTmToDouble

propagateWithUnBoolOp :: (Bool -> a) -> (a -> Tm) -> Tm -> Maybe Tm
propagateWithUnBoolOp = propagateWithUnConstOp boolTmToBool

propagateWithUnConstOp :: (Tm -> Maybe c) -> (c -> a) -> (a -> Tm) -> Tm -> Maybe Tm
propagateWithUnConstOp getC comp wrap tm = wrap . comp <$> getC tm

intTmToInt :: Tm -> Maybe Int
intTmToInt (TmInt d) = Just d
intTmToInt _         = Nothing

doubleTmToDouble :: Tm -> Maybe Double
doubleTmToDouble (TmDouble d) = Just d
doubleTmToDouble _            = Nothing

boolTmToBool :: Tm -> Maybe Bool
boolTmToBool TmTrue  = Just True
boolTmToBool TmFalse = Just False
boolTmToBool _       = Nothing

boolToTm :: Bool -> Tm
boolToTm True  = TmTrue
boolToTm False = TmFalse
