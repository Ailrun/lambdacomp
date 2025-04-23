{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver where

import Control.Monad                (void)
import Paths_lambdacomp
import System.FilePath              ((<.>), takeBaseName)
import System.IO                    (hFlush, hPutStr)
import System.IO.Temp               (withSystemTempFile)
import System.Process               (rawSystem)

import LambdaComp.AM.Eval           (topEval)
import LambdaComp.CBPV.Optimization (topOptimizeDefault)
import LambdaComp.CBPV.ToAM         (runToAM)
import LambdaComp.CBPV.ToC          (runToC)
import LambdaComp.CBV.ToCBPV        (runToCBPV)
import LambdaComp.Syntax

test0 :: Tm
test0 = TmPrintInt (TmLam "x" ("x" `TmApp` TmInt 5) `TmApp` TmLam "x" "x") $ TmInt 0

test1 :: Tm
test1 = TmPrintInt (TmLam "x" (TmLam "y" (TmPrintInt "y" "x") `TmApp` TmInt 2) `TmApp` TmPrintInt (TmInt 7) (TmInt 3)) $ TmInt 0

test2 :: Tm
test2 = TmRec "f" (TmLam "x" $ TmPrintInt "x" $ TmPrintInt (TmInt 2) $ "f" `TmApp` "x") `TmApp` TmInt 3

mainForC :: Tm -> FilePath -> IO ()
mainForC tm fp = do
  dataDir <- getDataDir
  void $ withSystemTempFile tempCFileName $ \tempCfp handle -> do
    hPutStr handle $ runToC $ topOptimizeDefault $ runToCBPV tm
    hFlush handle
    rawSystem "gcc" ["-O2", "-I", dataDir, "-o", fp <> ".out", tempCfp]
  where
    tempCFileName = takeBaseName fp <.> "temp" <.> "c"

mainForAM :: Tm -> IO ()
mainForAM tm = do
  it <- topEval $ runToAM $ topOptimizeDefault $ runToCBPV tm
  print it

