{-# LANGUAGE OverloadedStrings #-}
module Main where

import LambdaComp.Syntax
import LambdaComp.ToCBPV
import LambdaComp.CBPV.ToC
import LambdaComp.CBPV.Optimization (topOptimizeDefault)

test0 :: Tm
test0 = TmPrintInt (TmLam "x" ("x" `TmApp` TmInt 5) `TmApp` TmLam "x" "x") $ TmInt 0

test1 :: Tm
test1 = TmPrintInt (TmLam "x" (TmLam "y" (TmPrintInt "y" "x") `TmApp` TmInt 2) `TmApp` TmPrintInt (TmInt 7) (TmInt 3)) $ TmInt 0

test2 :: Tm
test2 = TmRec "f" (TmLam "x" $ TmPrintInt "x" $ TmPrintInt (TmInt 2) $ "f" `TmApp` "x") `TmApp` TmInt 3

allSteps :: Tm -> FilePath -> IO ()
allSteps tm fp = writeFile fp $ runToC $ topOptimizeDefault $ runToCBPV tm

main :: IO ()
main = do
  allSteps test0 "./output/test0.c"
  allSteps test1 "./output/test1.c"
  allSteps test2 "./output/test2.c"
