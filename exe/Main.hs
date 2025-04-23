{-# LANGUAGE OverloadedStrings #-}
module Main where

import LambdaComp.Driver (mainForAM, mainForC)
import LambdaComp.Syntax

test0 :: Tm
test0 = TmPrintInt (TmLam "x" ("x" `TmApp` TmInt 5) `TmApp` TmLam "x" "x") $ TmInt 0

test1 :: Tm
test1 = TmPrintInt (TmLam "x" (TmLam "y" (TmPrintInt "y" "x") `TmApp` TmInt 2) `TmApp` TmPrintInt (TmInt 7) (TmInt 3)) $ TmInt 0

test2 :: Tm
test2 = TmRec "f" (TmLam "x" $ TmPrintInt "x" $ TmPrintInt (TmInt 2) $ "f" `TmApp` "x") `TmApp` TmInt 3

main :: IO ()
main = do
  mainForC test0 "output/test0"
  mainForC test1 "output/test1"
  mainForC test2 "output/test2"
  mainForAM test0
  mainForAM test1
  mainForAM test2
