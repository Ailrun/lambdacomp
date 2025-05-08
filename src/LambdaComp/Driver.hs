{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver where

import Paths_lambdacomp (getDataDir)

import Control.Monad      (void)
import Data.Text.IO       qualified as T
import System.Directory   (makeAbsolute)
import System.Exit        (ExitCode (ExitFailure), exitWith)
import System.FilePath    (takeBaseName, (<.>), (</>))
import System.IO          (hFlush, hPutStr)
import System.IO.Temp     (withSystemTempDirectory, withSystemTempFile)
import System.Process     (createProcess, proc, rawSystem, waitForProcess)
import Text.Pretty.Simple (pPrintNoColor)

import LambdaComp.AM.Eval                (topEval)
import LambdaComp.CBPV.LocalOptimization (runLocalOptDefault)
import LambdaComp.CBPV.ToAM              (runToAM)
import LambdaComp.CBPV.ToC               (runToC)
import LambdaComp.Driver.Argument
import LambdaComp.Elaborated.CBV.ToCBPV  (runToCBPV)
import LambdaComp.Elaborated.Syntax      qualified as E
import LambdaComp.External.ToElaborated  (ElaborationError, runToElaborated)
import LambdaComp.Parser                 (runProgramParser)

mainFunc :: IO ()
mainFunc = do
  Options inputFp backend phase mayFp <- parseOptions
  input <- T.readFile inputFp
  case runProgramParser inputFp input of
    Left err -> putStrLn err
    Right tm -> do
      let getElTm      = handleElabError $ runToElaborated tm
          getCBPVTm    = runToCBPV <$> getElTm
          getCBPVOptTm = runLocalOptDefault <$> getCBPVTm
          getCCode     = runToC <$> getCBPVOptTm
          getAMTm      = runToAM <$> getCBPVOptTm
      case phase of
        UntilAST         -> pPrintNoColor tm
        UntilElaboration -> getElTm >>= pPrintNoColor
        UntilCBPV        -> getCBPVTm >>= pPrintNoColor
        UntilCBPVOpt     -> getCBPVOptTm >>= pPrintNoColor
        UntilC           -> getCCode >>= (\cCode -> requireFp phase mayFp >>= makeAbsolute >>= (`writeFile` cCode))
        UntilExe         -> getCCode >>= (\cCode -> requireFp phase mayFp >>= makeAbsolute >>= genCExe cCode)
        UntilAM          -> getAMTm >>= pPrintNoColor
        Run              ->
          case backend of
            DirectCBackend -> do
              case mayFp of
                Just fp -> getCCode >>= (\cCode -> makeAbsolute fp >>= genAndExeCExe cCode)
                Nothing -> getCCode >>= withLambdaCompTempFile . genAndExeCExe
            AMBackend      -> getAMTm >>= topEval >>= pPrintNoColor

handleElabError :: Either ElaborationError E.Program -> IO E.Program
handleElabError (Left elabErr) = pPrintNoColor elabErr >> exitWith (ExitFailure 1)
handleElabError (Right prog)   = pure prog

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
