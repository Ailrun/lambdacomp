{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeData               #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module LambdaComp.Driver.Argument
  ( BackendType(..)
  , Backend(..)
  , Phase(..)
  , FilePathFor
  , Options(..)

  , parseOptions

  , printHelpForError
  , showPhaseOption
  ) where

import Options.Applicative

type data BackendType where
  DirectCBackendType, AMBackendType :: BackendType

data Backend (c :: BackendType) where
  DirectCBackend :: Backend DirectCBackendType
  AMBackend :: Backend AMBackendType

data Phase (c :: BackendType) where
  UntilAST         :: Phase c
  UntilElaboration :: Phase c
  UntilCBPV        :: Phase c
  UntilCBPVOpt     :: Phase c
  UntilC           :: Phase DirectCBackendType
  UntilExe         :: Phase DirectCBackendType
  UntilAM          :: Phase AMBackendType
  Run              :: Phase c

type family FilePathFor (c :: BackendType) = r | r -> c where
  FilePathFor DirectCBackendType = Maybe FilePath
  FilePathFor AMBackendType      = ()

data Options where
  Options :: { input :: FilePath, pass :: Backend c, phase :: Phase c, output :: FilePathFor c } -> Options

parseOptions :: IO Options
parseOptions = execParser progInfo

printHelpForError :: String -> IO a
printHelpForError h = handleParseResult . Failure $ parserFailure (prefs showHelpOnError) progInfo (ErrorMsg h) mempty

showPhaseOption :: Phase c -> String
showPhaseOption UntilAST         = "--until-ast option"
showPhaseOption UntilElaboration = "--until-elaboration option"
showPhaseOption UntilCBPV        = "--until-cbpv option"
showPhaseOption UntilCBPVOpt     = "--until-cbpv-opt option"
showPhaseOption UntilC           = "--until-c option"
showPhaseOption UntilExe         = "--until-exe option"
showPhaseOption UntilAM          = "--until-am option"
showPhaseOption Run              = "--run option"

progInfo :: ParserInfo Options
progInfo = info (getOptions <**> helper)
  $ progDesc "A primitive λ-calculus (+ α) compiler using CBPV"
  <> failureCode 1

getOptions :: Parser Options
getOptions = getInputFilePath <**> (getOptionsForDirectCBackend <|> getOptionsForAMBackend)

getInputFilePath :: Parser FilePath
getInputFilePath = strArgument (metavar "INPUT_FILE" <> help "The path of an input file." <> action "file")

getOptionsForDirectCBackend :: Parser (FilePath -> Options)
getOptionsForDirectCBackend = (\backend phase fp input -> Options input backend phase fp)
  <$> getDirectCBackend
  <*> getDirectCPhase
  <*> optional getOutputFilePath

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
                    <> help "Stop after generating a C and write that to the given output path. Available only for Direct-C backend.")
  <|> flag' UntilExe (long "until-exe"
                      <> hidden
                      <> help "Stop after generating an executable using C, which is written to the given output path. Currently available only for the direct-c backend. This is the default for the Direct-C backend.")
  <|> pure UntilExe

getOptionsForAMBackend :: Parser (FilePath -> Options)
getOptionsForAMBackend = (\backend phase input -> Options input backend phase ())
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
                     <> help "Stop after generating an abstract machine (AM) term and print it. Available only for the AM backend. This is the default for the AM backend.")
  <|> pure UntilAM

getCommonPhase :: Parser (Phase c)
getCommonPhase =
  flag' UntilAST (long "until-ast"
                   <> hidden
                   <> help "Stop after parsing an AST and print it.")
  <|> flag' UntilElaboration (long "until-elaboration"
                              <> hidden
                              <> help "Stop after generating an elaborated AST and print it.")
  <|> flag' UntilCBPV (long "until-cbpv"
                   <> hidden
                   <> help "Stop after generating a CBPV term and print it.")
  <|> flag' UntilCBPVOpt (long "until-cbpv-opt"
                          <> hidden
                          <> help "Stop after optimizing a CBPV term and print it.")
  <|> flag' Run (long "run"
                 <> short 'r'
                 <> help "Run the example term using the given backend. For the Direct-C backend, this generates and executes an executable using C, which is written to the given output path. For the AM backend, this interprets an abstract machine (AM) term and prints the result value.")

getOutputFilePath :: Parser FilePath
getOutputFilePath =
  strOption
  $ long "output"
  <> short 'o'
  <> metavar "OUTPUT_FILE"
  <> action "file"
  <> help "Set the output path to OUTPUT_FILE."
