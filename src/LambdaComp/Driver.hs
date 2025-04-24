{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver where

import Paths_lambdacomp (getDataDir)

import Control.Monad    (void)
import System.Directory (makeAbsolute)
import System.FilePath  (takeBaseName, (<.>))
import System.IO        (hFlush, hPutStr)
import System.IO.Temp   (withSystemTempFile)
import System.Process   (callProcess, rawSystem)

import LambdaComp.AM.Eval           (topEval)
import LambdaComp.CBPV.Optimization (topOptimizeDefault)
import LambdaComp.CBPV.ToAM         (runToAM)
import LambdaComp.CBPV.ToC          (runToC)
import LambdaComp.CBV.ToCBPV        (runToCBPV)
import LambdaComp.Driver.Argument   (Backend (AMBackend, DirectCBackend), Options (Options),
                                     Phase (Run, UntilAM, UntilC, UntilCBPV, UntilCBPVOpt, UntilExe), parseOptions)
import LambdaComp.Syntax            (Tm)

mainFunc :: IO ()
mainFunc = do
  Options tm backend phase fp <- parseOptions
  let cbpvTm    = runToCBPV tm
      cbpvOptTm = topOptimizeDefault cbpvTm
      cCode     = runToC cbpvOptTm
      amTm      = runToAM cbpvOptTm
  case phase of
    UntilCBPV    -> print cbpvTm
    UntilCBPVOpt -> print cbpvOptTm
    UntilC       -> makeAbsolute fp >>= (`writeFile` cCode)
    UntilExe     -> generateExe tm fp
    UntilAM      -> print amTm
    Run          ->
      case backend of
        DirectCBackend -> generateExe tm fp >> makeAbsolute fp >>= (`callProcess` [])
        AMBackend      -> topEval amTm >>= print

generateExe :: Tm -> FilePath -> IO ()
generateExe tm fp = do
  dataDir <- getDataDir
  let tempCFileName = takeBaseName fp <.> "c"
  void $ withSystemTempFile tempCFileName $ \tempCfp handle -> do
    hPutStr handle $ runToC $ topOptimizeDefault $ runToCBPV tm
    hFlush handle
    absfp <- makeAbsolute fp
    rawSystem "gcc" ["-O2", "-I", dataDir, "-o", absfp, tempCfp]
