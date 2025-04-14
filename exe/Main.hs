{-# LANGUAGE OverloadedStrings #-}
module Main where

import LambdaComp.Syntax
import LambdaComp.ToCBPV
import LambdaComp.CBPV.ToC

main :: IO ()
main = do
  let cbpv = runToCBPV (TmPrint (TmLam "x" (TmVar "x" `TmApp` TmInt 5) `TmApp` TmLam "x" (TmVar "x")) (TmInt 0))
      c = runToC cbpv
  writeFile "./test.c" c
