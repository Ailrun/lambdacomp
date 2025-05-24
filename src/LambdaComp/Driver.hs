module LambdaComp.Driver
  ( mainFuncWithOptions
  ) where

import Paths_lambdacomp (getDataDir)

import Control.Exception    (SomeException, bracketOnError, catch)
import Control.Monad.Except (ExceptT (ExceptT, runExceptT), MonadError (throwError))
import Control.Monad.Trans  (MonadTrans (lift))
import Data.Functor         ((<&>))
import Data.Text.IO         qualified as T
import System.Directory     (makeAbsolute)
import System.Exit          (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath      (takeBaseName, (<.>), (</>))
import System.IO            (Handle, hFlush, hPutStr, hPutStrLn, stderr)
import System.IO.Temp       (withSystemTempDirectory, withSystemTempFile)
import System.Process       (CreateProcess (..), StdStream (..), cleanupProcess, createProcess_, proc, waitForProcess)
import Text.Pretty.Simple   (pHPrintNoColor)

import LambdaComp.AM.Eval                       (topEval)
import LambdaComp.CBPV.ArityAnalysis            (runArityAnalysis)
import LambdaComp.CBPV.Optimization.Local       qualified as CBPV
import LambdaComp.CBPV.ToAM                     (runToAM)
import LambdaComp.CBPV.ToC                      (runToC)
import LambdaComp.CBPV.TypeCheck                qualified as CBPV
import LambdaComp.CBPV.PrettyPrinter            ()
import LambdaComp.Driver.Argument
import LambdaComp.Elaborated.CBV.ToCBPV         (runToCBPV)
import LambdaComp.Elaborated.Optimization.Local qualified as El
import LambdaComp.Elaborated.TypeCheck          qualified as El
import LambdaComp.External.Parser               (runProgramParser)
import LambdaComp.External.ToElaborated         (ElaborationError, runToElaborated)
import Prettyprinter.Render.Text (hPutDoc)
import Prettyprinter (Pretty(pretty))

mainFuncWithOptions :: Handle -> Options -> IO ExitCode
mainFuncWithOptions outH (Options inputFp backend phase mayFp) = (<* hFlush outH) . exceptTToExitCode $ do
  input <- lift $ T.readFile inputFp
  let getTm          = either ((>> throwError 1) . lift . hPutStrLn stderr) pure $ runProgramParser inputFp input
      getElTm        = getTm >>= handleElabError outH . runToElaborated
      getElTmTc      = getElTm >>= \p -> p <$ handleElTcError outH (El.runProgramInfer p)
      getElOptTm     = El.runLocalOptDefault <$> getElTmTc
      getElOptTmTc   = getElOptTm >>= \p -> p <$ handleElTcError outH (El.runProgramInfer p)
      getCBPVTm      = runToCBPV <$> getElOptTmTc
      getCBPVTmTc    = getCBPVTm >>= \p -> p <$ handleCBPVTcError outH (CBPV.runProgramInfer p)
      getCBPVTmAA    = runArityAnalysis <$> getCBPVTmTc
      getCBPVTmAATc  = getCBPVTmAA >>= \p -> p <$ handleCBPVTcError outH (CBPV.runProgramInfer p)
      getCBPVOptTm   = CBPV.runLocalOptDefault <$> getCBPVTmAATc
      getCBPVOptTmTc = getCBPVOptTm >>= \p -> p <$ handleCBPVTcError outH (CBPV.runProgramInfer p)
      getCCode       = runToC <$> getCBPVOptTmTc
      getAMTm        = runToAM <$> getCBPVOptTmTc
  case phase of
    UntilAST            -> getTm >>= pHPrintNoColor outH
    UntilElaboration    -> getElTmTc >>= pHPrintNoColor outH
    UntilElaborationOpt -> getElOptTmTc >>= pHPrintNoColor outH
    UntilCBPV           -> getCBPVTmAATc >>= lift . hPutDoc outH . pretty
    UntilCBPVOpt        -> getCBPVOptTmTc >>= lift . hPutDoc outH . pretty
    UntilC              -> getCCode >>= (\cCode -> runWithFp (\real -> lift . if real then (`writeFile` cCode) else const $ hPutStr outH cCode) mayFp)
    UntilExe            -> getCCode >>= (\cCode -> runWithFp (const $ genCExe outH cCode) mayFp)
    UntilAM             -> getAMTm >>= pHPrintNoColor outH
    Run                 ->
      case backend of
        DirectCBackend -> do
          cCode <- getCCode
          runWithFp (const $ genAndExeCExe outH cCode) mayFp
        AMBackend      -> getAMTm >>= exitCodeToExceptT . topEval outH

handleElabError :: Handle -> Either ElaborationError a -> ExceptT Int IO a
handleElabError outH (Left elabErr) = lift (hPutStrLn outH "Elab") >> pHPrintNoColor outH elabErr >> throwError 1
handleElabError _    (Right prog)   = pure prog

handleElTcError :: Handle -> Either El.TypeError a -> ExceptT Int IO a
handleElTcError outH (Left elTcErr) = lift (hPutStrLn outH "ElTc") >> pHPrintNoColor outH elTcErr >> throwError 1
handleElTcError _    (Right prog)   = pure prog

handleCBPVTcError :: Handle -> Either CBPV.TypeError a -> ExceptT Int IO a
handleCBPVTcError outH (Left cbpvTcErr) = lift (hPutStrLn outH "CBPVTc") >> pHPrintNoColor outH cbpvTcErr >> throwError 1
handleCBPVTcError _    (Right prog)     = pure prog

runWithFp :: (Bool -> FilePath -> ExceptT Int IO a) -> Maybe FilePath -> ExceptT Int IO a
runWithFp f (Just fp) = lift (makeAbsolute fp) >>= f True
runWithFp f Nothing   = withLambdaCompTempFile $ f False

withLambdaCompTempFile :: (FilePath -> ExceptT Int IO a) -> ExceptT Int IO a
withLambdaCompTempFile f =
  withSystemTempDirectory "lambdacomp_direct_c" $ f . (</> "temp.exe")

-- requireFp :: Phase c -> Maybe FilePath -> IO FilePath
-- requireFp _      (Just fp) = pure fp
-- requireFp phase  Nothing   = printHelpForError $ showPhaseOption phase <> " requires output file path."

genAndExeCExe :: Handle -> String -> FilePath -> ExceptT Int IO ()
genAndExeCExe outH cCode fp = do
  genCExe outH cCode fp
  exitCodeToExceptT $ bracketOnError
    (createProcess_ "genAndExeCExe" (proc fp []) { std_out = UseHandle outH })
    cleanupProcess
    (\(_, _, _, p) -> waitForProcess p `catch` (\(_ :: SomeException) -> pure ExitSuccess))

genCExe :: Handle -> String -> FilePath -> ExceptT Int IO ()
genCExe outH cCode fp = do
  dataDir <- lift getDataDir
  let tempCFileName = takeBaseName fp <.> "c"
  withSystemTempFile tempCFileName $ \tempCfp handle -> do
    lift $ hPutStr handle cCode
    lift $ hFlush handle
    exitCodeToExceptT $ bracketOnError
      (createProcess_ "genCExe" (proc "gcc" ["-O2", "-I", dataDir, "-o", fp, tempCfp]) { std_out = UseHandle outH })
      cleanupProcess
      (\(_, _, _, p) -> waitForProcess p `catch` (\(_ :: SomeException) -> pure ExitSuccess))

exitCodeToExceptT :: Functor m => m ExitCode -> ExceptT Int m ()
exitCodeToExceptT a = ExceptT $ a <&> \case
  ExitSuccess -> Right ()
  ExitFailure c -> Left c

exceptTToExitCode :: Functor m => ExceptT Int m () -> m ExitCode
exceptTToExitCode = fmap (either ExitFailure (const ExitSuccess)) . runExceptT
