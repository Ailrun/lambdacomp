{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver where

import Paths_lambdacomp (getDataDir)

import Control.Monad      (void)
import System.Directory   (makeAbsolute)
import System.Exit        (exitWith)
import System.FilePath    (takeBaseName, (<.>))
import System.IO          (hFlush, hPutStr)
import System.IO.Temp     (withSystemTempFile)
import System.Process     (createProcess, proc, rawSystem, waitForProcess)
import Text.Pretty.Simple (pPrintNoColor)

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
    UntilCBPV    -> pPrintNoColor cbpvTm
    UntilCBPVOpt -> pPrintNoColor cbpvOptTm
    UntilC       -> makeAbsolute fp >>= (`writeFile` cCode)
    UntilExe     -> makeAbsolute fp >>= generateExe tm fp
    UntilAM      -> pPrintNoColor amTm
    Run          ->
      case backend of
        DirectCBackend -> do
          absFp <- makeAbsolute fp
          generateExe tm fp absFp
          executeExe absFp
        AMBackend      -> topEval amTm >>= pPrintNoColor

generateExe :: Tm -> FilePath -> FilePath -> IO ()
generateExe tm fp absFp = do
  dataDir <- getDataDir
  let tempCFileName = takeBaseName fp <.> "c"
  void $ withSystemTempFile tempCFileName $ \tempCfp handle -> do
    hPutStr handle $ runToC $ topOptimizeDefault $ runToCBPV tm
    hFlush handle
    rawSystem "gcc" ["-O2", "-I", dataDir, "-o", absFp, tempCfp]

executeExe :: FilePath -> IO ()
executeExe absFp = do
  (_, _, _, handle) <- createProcess (proc absFp [])
  e <- waitForProcess handle
  exitWith e
