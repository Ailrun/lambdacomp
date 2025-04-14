{-# LANGUAGE OverloadedStrings #-}
module Main where

import LambdaComp.Syntax
import LambdaComp.ToCBPV
import LambdaComp.CBPV.ToC

test0 :: Tm
test0 = TmPrint (TmLam "x" (TmVar "x" `TmApp` TmInt 5) `TmApp` TmLam "x" (TmVar "x")) (TmInt 0)

test1 :: Tm
test1 = TmPrint (TmLam "x" (TmLam "y" (TmPrint (TmVar "y") (TmVar "x")) `TmApp` TmInt 2) `TmApp` TmPrint (TmInt 7) (TmInt 3)) (TmInt 0)

allSteps :: Tm -> FilePath -> IO ()
allSteps tm fp = writeFile fp $ runToC $ runToCBPV tm

main :: IO ()
main = do
  allSteps test0 "./output/test0.c"
  allSteps test1 "./output/test1.c"
