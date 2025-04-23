{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver where

import Paths_lambdacomp (getDataDir)

import Control.Monad   (void)
import System.FilePath (takeBaseName, (<.>))
import System.IO       (hFlush, hPutStr)
import System.IO.Temp  (withSystemTempFile)
import System.Process  (rawSystem)

import LambdaComp.AM.Eval           (topEval)
import LambdaComp.CBPV.Optimization (topOptimizeDefault)
import LambdaComp.CBPV.ToAM         (runToAM)
import LambdaComp.CBPV.ToC          (runToC)
import LambdaComp.CBV.ToCBPV        (runToCBPV)
import LambdaComp.Syntax            (Tm)

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

