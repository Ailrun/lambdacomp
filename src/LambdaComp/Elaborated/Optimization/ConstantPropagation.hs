module LambdaComp.Elaborated.Optimization.ConstantPropagation
  ( runConstantsPropagation
  ) where

import LambdaComp.Elaborated.Syntax

runConstantsPropagation :: Tm -> Tm
runConstantsPropagation = propagateConstants

propagateConstants :: Tm -> Tm
propagateConstants tm@(TmVar _)             = tm
propagateConstants tm@(TmGlobal _)          = tm
propagateConstants tm@(TmConst _)           = tm
propagateConstants (TmIf tm0 tm1 tm2)       = TmIf (propagateConstants tm0) (propagateConstants tm1) (propagateConstants tm2)
propagateConstants (TmLam ps tm)            = TmLam ps $ propagateConstants tm
propagateConstants (tmf `TmApp` tma)        = propagateConstants tmf `TmApp` propagateConstants tma
propagateConstants (TmPrimBinOp op tm0 tm1) = maybe (TmPrimBinOp op tm0' tm1') TmConst $ propagateWithBinOp op tm0' tm1'
  where
    tm0' = propagateConstants tm0
    tm1' = propagateConstants tm1
propagateConstants (TmPrimUnOp op tm)       = maybe (TmPrimUnOp op tm') TmConst $ propagateWithUnOp op tm
  where
    tm' = propagateConstants tm
propagateConstants (TmPrintInt tm0 tm1)     = TmPrintInt (propagateConstants tm0) (propagateConstants tm1)
propagateConstants (TmPrintDouble tm0 tm1)  = TmPrintDouble (propagateConstants tm0) (propagateConstants tm1)
propagateConstants (TmRec p tm)             = TmRec p (propagateConstants tm)

propagateWithBinOp :: PrimOp Binary -> Tm -> Tm -> Maybe TmConst
propagateWithBinOp PrimIAdd = propagateWithBinIntOp (+) TmCInt
propagateWithBinOp PrimISub = propagateWithBinIntOp (-) TmCInt
propagateWithBinOp PrimIMul = propagateWithBinIntOp (*) TmCInt
propagateWithBinOp PrimIDiv = propagateWithBinIntOp quot TmCInt
propagateWithBinOp PrimIMod = propagateWithBinIntOp rem TmCInt
propagateWithBinOp PrimIEq  = propagateWithBinIntOp (==) boolToTmConst
propagateWithBinOp PrimINEq = propagateWithBinIntOp (/=) boolToTmConst
propagateWithBinOp PrimILt  = propagateWithBinIntOp (<) boolToTmConst
propagateWithBinOp PrimILe  = propagateWithBinIntOp (<=) boolToTmConst
propagateWithBinOp PrimIGt  = propagateWithBinIntOp (>) boolToTmConst
propagateWithBinOp PrimIGe  = propagateWithBinIntOp (>=) boolToTmConst
propagateWithBinOp PrimDAdd = propagateWithBinDoubleOp (+) TmCDouble
propagateWithBinOp PrimDSub = propagateWithBinDoubleOp (-) TmCDouble
propagateWithBinOp PrimDMul = propagateWithBinDoubleOp (*) TmCDouble
propagateWithBinOp PrimDDiv = propagateWithBinDoubleOp (/) TmCDouble
propagateWithBinOp PrimDEq  = propagateWithBinDoubleOp (==) boolToTmConst
propagateWithBinOp PrimDNEq = propagateWithBinDoubleOp (/=) boolToTmConst
propagateWithBinOp PrimDLt  = propagateWithBinDoubleOp (<) boolToTmConst
propagateWithBinOp PrimDLe  = propagateWithBinDoubleOp (<=) boolToTmConst
propagateWithBinOp PrimDGt  = propagateWithBinDoubleOp (>) boolToTmConst
propagateWithBinOp PrimDGe  = propagateWithBinDoubleOp (>=) boolToTmConst
propagateWithBinOp PrimBAnd = propagateWithBinBoolOp (&&) boolToTmConst
propagateWithBinOp PrimBOr  = propagateWithBinBoolOp (||) boolToTmConst

propagateWithBinIntOp :: (Int -> Int -> a) -> (a -> TmConst) -> Tm -> Tm -> Maybe TmConst
propagateWithBinIntOp = propagateWithBinConstOp intTmToInt

propagateWithBinDoubleOp :: (Double -> Double -> a) -> (a -> TmConst) -> Tm -> Tm -> Maybe TmConst
propagateWithBinDoubleOp = propagateWithBinConstOp doubleTmToDouble

propagateWithBinBoolOp :: (Bool -> Bool -> a) -> (a -> TmConst) -> Tm -> Tm -> Maybe TmConst
propagateWithBinBoolOp = propagateWithBinConstOp boolTmToBool

propagateWithBinConstOp :: (Tm -> Maybe c) -> (c -> c -> a) -> (a -> TmConst) -> Tm -> Tm -> Maybe TmConst
propagateWithBinConstOp getC comp wrap tm0 tm1 = wrap <$> (comp <$> getC tm0 <*> getC tm1)

propagateWithUnOp :: PrimOp Unary -> Tm -> Maybe TmConst
propagateWithUnOp PrimINeg = propagateWithUnIntOp negate TmCInt
propagateWithUnOp PrimIToD = propagateWithUnIntOp fromIntegral TmCDouble
propagateWithUnOp PrimDNeg = propagateWithUnDoubleOp negate TmCDouble
propagateWithUnOp PrimDToI = propagateWithUnDoubleOp truncate TmCInt
propagateWithUnOp PrimBNot = propagateWithUnBoolOp not boolToTmConst

propagateWithUnIntOp :: (Int -> a) -> (a -> TmConst) -> Tm -> Maybe TmConst
propagateWithUnIntOp = propagateWithUnConstOp intTmToInt

propagateWithUnDoubleOp :: (Double -> a) -> (a -> TmConst) -> Tm -> Maybe TmConst
propagateWithUnDoubleOp = propagateWithUnConstOp doubleTmToDouble

propagateWithUnBoolOp :: (Bool -> a) -> (a -> TmConst) -> Tm -> Maybe TmConst
propagateWithUnBoolOp = propagateWithUnConstOp boolTmToBool

propagateWithUnConstOp :: (Tm -> Maybe c) -> (c -> a) -> (a -> TmConst) -> Tm -> Maybe TmConst
propagateWithUnConstOp getC comp wrap tm = wrap . comp <$> getC tm

intTmToInt :: Tm -> Maybe Int
intTmToInt (TmConst (TmCInt d)) = Just d
intTmToInt _                    = Nothing

doubleTmToDouble :: Tm -> Maybe Double
doubleTmToDouble (TmConst (TmCDouble d)) = Just d
doubleTmToDouble _                       = Nothing

boolTmToBool :: Tm -> Maybe Bool
boolTmToBool (TmConst TmCTrue)  = Just True
boolTmToBool (TmConst TmCFalse) = Just False
boolTmToBool _                  = Nothing

boolToTmConst :: Bool -> TmConst
boolToTmConst True  = TmCTrue
boolToTmConst False = TmCFalse
