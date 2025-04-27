{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver.Example where

import LambdaComp.Syntax

examples :: [Program]
examples =
  [ example0
  , example1
  , example2
  , example3
  , example4
  ]

example0 :: Program
example0 = termToMainProg $ TmPrintInt (TmLam "x" ("x" `TmApp` TmInt 5) `TmApp` TmLam "x" "x") $ TmInt 0

example1 :: Program
example1 = termToMainProg $ TmPrintInt (TmLam "x" (TmLam "y" (TmPrintInt "y" "x") `TmApp` TmInt 2) `TmApp` TmPrintInt (TmInt 7) (TmInt 3)) $ TmInt 0

example2 :: Program
example2 = termToMainProg $ TmRec "f" (TmLam "x" $ TmPrintInt "x" $ TmPrintInt (TmInt 2) $ "f" `TmApp` "x") `TmApp` TmInt 3

example3 :: Program
example3 = termToMainProg $ TmIf (TmLam "x" "x" `TmApp` TmTrue) (TmPrintInt (TmInt 1) $ TmInt 1) (TmPrintInt (TmInt 0) $ TmInt 0)

example4 :: Program
example4 = termToMainProg $ TmLam "n" (((TmRec "f" (TmLam "l" $ TmLam "x" $ TmLam "y" $ TmIf (TmPrimBinOp PrimILt "n" "l") (TmInt 0) (TmPrintInt "l" $ TmPrintInt "x" $ (("f" `TmApp` intAddTerm "l" (TmInt 1)) `TmApp` "y") `TmApp` intAddTerm "x" "y")) `TmApp` TmInt 0) `TmApp` TmInt 0) `TmApp` TmInt 1) `TmApp` TmInt 10

termToMainProg :: Tm -> Program
termToMainProg = pure . TopTmDef "main" TpInt

intAddTerm :: Tm -> Tm -> Tm
intAddTerm = TmPrimBinOp PrimIAdd
