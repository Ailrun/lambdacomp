{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver where

import Paths_lambdacomp (getDataDir)

import Control.Monad      (void)
import System.Directory   (makeAbsolute)
import System.Exit        (exitWith)
import System.FilePath    (takeBaseName, (<.>), (</>))
import System.IO          (hFlush, hPutStr)
import System.IO.Temp     (withSystemTempDirectory, withSystemTempFile)
import System.Process     (createProcess, proc, rawSystem, waitForProcess)
import Text.Pretty.Simple (pPrintNoColor)

import LambdaComp.AM.Eval                (topEval)
import LambdaComp.CBPV.LocalOptimization (runLocalOptDefault)
import LambdaComp.CBPV.ToAM              (runToAM)
import LambdaComp.CBPV.ToC               (runToC)
import LambdaComp.CBV.ToCBPV             (runToCBPV)
import LambdaComp.Driver.Argument

mainFunc :: IO ()
mainFunc = do
  Options tm backend phase mayFp <- parseOptions
  let cbpvTm    = runToCBPV tm
      cbpvOptTm = runLocalOptDefault cbpvTm
      cCode     = runToC cbpvOptTm
      amTm      = runToAM cbpvOptTm
  case phase of
    UntilCBPV    -> pPrintNoColor cbpvTm
    UntilCBPVOpt -> pPrintNoColor cbpvOptTm
    UntilC       -> requireFp phase mayFp >>= makeAbsolute >>= (`writeFile` cCode)
    UntilExe     -> requireFp phase mayFp >>= makeAbsolute >>= genCExe cCode
    UntilAM      -> pPrintNoColor amTm
    Run          ->
      case backend of
        DirectCBackend -> do
          case mayFp of
            Just fp -> makeAbsolute fp >>= genAndExeCExe cCode
            Nothing -> withLambdaCompTempFile $ genAndExeCExe cCode
        AMBackend      -> topEval amTm >>= pPrintNoColor

withLambdaCompTempFile :: (FilePath -> IO a) -> IO a
withLambdaCompTempFile f =
  withSystemTempDirectory "lambdacomp_direct_c" $ f . (</> "temp.exe")

requireFp :: Phase c -> Maybe FilePath -> IO FilePath
requireFp _      (Just fp) = pure fp
requireFp phase  Nothing   = printHelpForError $ showPhaseOption phase <> " requires output file path."

genAndExeCExe :: String -> FilePath -> IO ()
genAndExeCExe cCode fp = do
  genCExe cCode fp
  (_, _, _, handle) <- createProcess (proc fp [])
  e <- waitForProcess handle
  exitWith e

genCExe :: String -> FilePath -> IO ()
genCExe cCode fp = do
  dataDir <- getDataDir
  let tempCFileName = takeBaseName fp <.> "c"
  void $ withSystemTempFile tempCFileName $ \tempCfp handle -> do
    hPutStr handle cCode
    hFlush handle
    rawSystem "gcc" ["-O2", "-I", dataDir, "-o", fp, tempCfp]
