{-# LANGUAGE TypeData               #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Driver.Argument where

import Options.Applicative
import LambdaComp.Syntax

type data BackendType where
  DirectCBackendType, AMBackendType :: BackendType

data Backend (c :: BackendType) where
  DirectCBackend :: Backend DirectCBackendType
  AMBackend :: Backend AMBackendType

data Phase (c :: BackendType) where
  UntilCBPV    :: Phase c
  UntilCBPVOpt :: Phase c
  UntilC       :: Phase DirectCBackendType
  UntilExe     :: Phase DirectCBackendType
  ExecuteExe   :: Phase DirectCBackendType
  UntilAM      :: Phase AMBackendType
  Interpret    :: Phase AMBackendType

type family FilePathFor (c :: BackendType) = r | r -> c where
  FilePathFor DirectCBackendType = FilePath
  FilePathFor AMBackendType      = ()

data Options where
  Options :: { testTm :: Tm, pass :: Backend c, phase :: Phase c, output :: FilePathFor c } -> Options

parseOptions :: IO Options
parseOptions =
  execParser
  $ info (getOptions <**> helper)
  $ progDesc "A primitive λ-calculus (+ α) compiler using CBPV"
  <> failureCode 1

getOptions :: Parser Options
getOptions = getTestTmArg <**> (getOptionsForDirectCBackend <|> getOptionsForAMBackend)

getTestTmArg :: Parser Tm
getTestTmArg = toTestTm <$> argument auto (metavar "EXAMPLE_ID" <> value 0 <> help "Use the example corresponding to the given number. The number should be 0, 1, or 2." <> completeWith ["0", "1", "2"])
  where
    toTestTm :: Int -> Tm
    toTestTm 0 = TmPrintInt (TmLam "x" ("x" `TmApp` TmInt 5) `TmApp` TmLam "x" "x") $ TmInt 0
    toTestTm 1 = TmPrintInt (TmLam "x" (TmLam "y" (TmPrintInt "y" "x") `TmApp` TmInt 2) `TmApp` TmPrintInt (TmInt 7) (TmInt 3)) $ TmInt 0
    toTestTm _ = TmRec "f" (TmLam "x" $ TmPrintInt "x" $ TmPrintInt (TmInt 2) $ "f" `TmApp` "x") `TmApp` TmInt 3

getOptionsForDirectCBackend :: Parser (Tm -> Options)
getOptionsForDirectCBackend = (\backend phase fp tm -> Options tm backend phase fp)
  <$> getDirectCBackend
  <*> getDirectCPhase
  <*> getFilePath

getDirectCBackend :: Parser (Backend DirectCBackendType)
getDirectCBackend =
  flag' DirectCBackend (long "direct-c"
                        <> short 'c'
                        <> help "Use the backend that goes directly to C.")

getDirectCPhase :: Parser (Phase DirectCBackendType)
getDirectCPhase =
  getCommonPhase
  <|> flag' UntilC (long "until-c"
                    <> hidden
                    <> help "Stop after generating a C and write that to the given output path. Available only for direct-c backend.")
  <|> flag' UntilExe (long "until-exe"
                      <> hidden
                      <> help "Stop after generating an executable using C, which is written to the given output path. Currently available only for the direct-c backend.")
  <|> flag' ExecuteExe (long "execute"
                        <> hidden
                        <> help "Generate and execute an executable using C, which is written to the given output path. Currently available only for the direct-c backend. This is the default for the direct-c backend.")
  <|> pure ExecuteExe

getOptionsForAMBackend :: Parser (Tm -> Options)
getOptionsForAMBackend = (\backend phase tm -> Options tm backend phase ())
  <$> getAMBackend
  <*> getAMPhase

getAMBackend :: Parser (Backend AMBackendType)
getAMBackend =
  flag' AMBackend (long "am"
                   <> help "Use the backend that goes through an abstract machine (AM). This is the default.")
  <|> pure AMBackend

getAMPhase :: Parser (Phase AMBackendType)
getAMPhase =
  getCommonPhase
  <|> flag' UntilAM (long "until-am"
                     <> hidden
                     <> help "Stop after generating an abstract machine (AM) term and print it. Available only for the AM backend.")
  <|> flag' Interpret (long "interpret"
                       <> hidden
                       <> help "Interpret an abstract machine (AM) term and print the result value. Available only for the AM backend. This is the default for the AM backend.")
  <|> pure Interpret

getCommonPhase :: Parser (Phase c)
getCommonPhase =
  flag' UntilCBPV (long "until-cbpv"
                   <> hidden
                   <> help "Stop after generating a CBPV term and print it.")
  <|> flag' UntilCBPVOpt (long "until-cbpv-opt"
                          <> hidden
                          <> help "Stop after optimizing a CBPV term and print it.")

getFilePath :: Parser FilePath
getFilePath =
  strOption
  $ long "output"
  <> short 'o'
  <> metavar "OUTPUT_FILE"
  <> action "file"
  <> help "Set the output path to OUTPUT_FILE."
