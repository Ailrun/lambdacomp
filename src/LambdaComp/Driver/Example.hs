{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver.Example where

import LambdaComp.Syntax

examples :: [Tm]
examples =
  [ TmPrintInt (TmLam "x" ("x" `TmApp` TmInt 5) `TmApp` TmLam "x" "x") $ TmInt 0
  , TmPrintInt (TmLam "x" (TmLam "y" (TmPrintInt "y" "x") `TmApp` TmInt 2) `TmApp` TmPrintInt (TmInt 7) (TmInt 3)) $ TmInt 0
  , TmRec "f" (TmLam "x" $ TmPrintInt "x" $ TmPrintInt (TmInt 2) $ "f" `TmApp` "x") `TmApp` TmInt 3
  , TmIf (TmLam "x" "x" `TmApp` TmTrue) (TmPrintInt (TmInt 1) $ TmInt 1) (TmPrintInt (TmInt 0) $ TmInt 0)
  , TmLam "n" (((TmRec "f" (TmLam "l" $ TmLam "x" $ TmLam "y" $ TmIf (TmPrimBinOp PrimILt "n" "l") (TmInt 0) (TmPrintInt "l" $ TmPrintInt "x" $ (("f" `TmApp` intAddTerm "l" (TmInt 1)) `TmApp` "y") `TmApp` intAddTerm "x" "y")) `TmApp` TmInt 0) `TmApp` TmInt 0) `TmApp` TmInt 1) `TmApp` TmInt 10
  ]

intAddTerm :: Tm -> Tm -> Tm
intAddTerm = TmPrimBinOp PrimIAdd
