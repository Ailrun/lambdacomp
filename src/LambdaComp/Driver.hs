{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver where

import Paths_lambdacomp (getDataDir)

import Control.Exception  (bracketOnError)
import Data.Text.IO       qualified as T
import System.Directory   (makeAbsolute)
import System.Exit        (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath    (takeBaseName, (<.>), (</>))
import System.IO          (Handle, hFlush, hPutStr, hPutStrLn, stderr)
import System.IO.Temp     (withSystemTempDirectory, withSystemTempFile)
import System.Process     (CreateProcess (..), StdStream (..), cleanupProcess, createProcess_, proc, waitForProcess)
import Text.Pretty.Simple (pHPrintNoColor)

import LambdaComp.AM.Eval                       (topEval)
import LambdaComp.CBPV.Optimization.Local       qualified as CBPV
import LambdaComp.CBPV.ToAM                     (runToAM)
import LambdaComp.CBPV.ToC                      (runToC)
import LambdaComp.Driver.Argument
import LambdaComp.Elaborated.CBV.ToCBPV         (runToCBPV)
import LambdaComp.Elaborated.Optimization.Local qualified as Elaborated
import LambdaComp.Elaborated.Syntax             qualified as E
import LambdaComp.External.ToElaborated         (ElaborationError, runToElaborated)
import LambdaComp.Parser                        (runProgramParser)

mainFuncWithOptions :: Handle -> Options -> IO ExitCode
mainFuncWithOptions outH (Options inputFp backend phase mayFp) = do
  input <- T.readFile inputFp
  e <- case runProgramParser inputFp input of
    Left err -> hPutStrLn stderr err >> pure (ExitFailure 1)
    Right tm -> do
      let getElTm      = handleElabError outH $ runToElaborated tm
          getElOptTm   = Elaborated.runLocalOptDefault <$> getElTm
          getCBPVTm    = runToCBPV <$> getElOptTm
          getCBPVOptTm = CBPV.runLocalOptDefault <$> getCBPVTm
          getCCode     = runToC <$> getCBPVOptTm
          getAMTm      = runToAM <$> getCBPVOptTm
      case phase of
        UntilAST            -> pHPrintNoColor outH tm >> pure ExitSuccess
        UntilElaboration    -> getElTm >>= pHPrintNoColor outH >> pure ExitSuccess
        UntilElaborationOpt -> getElOptTm >>= pHPrintNoColor outH >> pure ExitSuccess
        UntilCBPV           -> getCBPVTm >>= pHPrintNoColor outH >> pure ExitSuccess
        UntilCBPVOpt        -> getCBPVOptTm >>= pHPrintNoColor outH >> pure ExitSuccess
        UntilC              -> getCCode >>= (\cCode -> runWithFp (\real -> if real then (`writeFile` cCode) else \_ -> hPutStr outH cCode) mayFp >> pure ExitSuccess)
        UntilExe            -> getCCode >>= (\cCode -> runWithFp (const $ genCExe outH cCode) mayFp)
        UntilAM             -> getAMTm >>= pHPrintNoColor outH >> pure ExitSuccess
        Run                 ->
          case backend of
            DirectCBackend -> do
              cCode <- getCCode
              runWithFp (const $ genAndExeCExe outH cCode) mayFp
            AMBackend      -> getAMTm >>= topEval outH >>= pHPrintNoColor outH >> pure ExitSuccess
  hFlush outH
  pure e

handleElabError :: Handle -> Either ElaborationError E.Program -> IO E.Program
handleElabError outH (Left elabErr) = pHPrintNoColor outH elabErr >> exitWith (ExitFailure 1)
handleElabError _    (Right prog)   = pure prog

runWithFp :: (Bool -> FilePath -> IO a) -> Maybe FilePath -> IO a
runWithFp f (Just fp) = makeAbsolute fp >>= f True
runWithFp f Nothing   = withLambdaCompTempFile $ f False

withLambdaCompTempFile :: (FilePath -> IO a) -> IO a
withLambdaCompTempFile f =
  withSystemTempDirectory "lambdacomp_direct_c" $ f . (</> "temp.exe")

-- requireFp :: Phase c -> Maybe FilePath -> IO FilePath
-- requireFp _      (Just fp) = pure fp
-- requireFp phase  Nothing   = printHelpForError $ showPhaseOption phase <> " requires output file path."

genAndExeCExe :: Handle -> String -> FilePath -> IO ExitCode
genAndExeCExe outH cCode fp = do
  e <- genCExe outH cCode fp
  case e of
    ExitFailure _ -> pure e
    ExitSuccess -> do
      bracketOnError
        (createProcess_ "genAndExeCExe" (proc fp []) { std_out = UseHandle outH })
        cleanupProcess
        (\(_, _, _, p) -> waitForProcess p)

genCExe :: Handle -> String -> FilePath -> IO ExitCode
genCExe outH cCode fp = do
  dataDir <- getDataDir
  let tempCFileName = takeBaseName fp <.> "c"
  withSystemTempFile tempCFileName $ \tempCfp handle -> do
    hPutStr handle cCode
    hFlush handle
    bracketOnError
      (createProcess_ "genCExe" (proc "gcc" ["-O2", "-I", dataDir, "-o", fp, tempCfp]) { std_out = UseHandle outH })
      cleanupProcess
      (\(_, _, _, p) -> waitForProcess p)
