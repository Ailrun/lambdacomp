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

import Data.Char                 (isUpperCase, toLower)
import Data.List                 (stripPrefix)
import Data.Set                  (Set)
import Data.Set                  qualified as Set
import Data.String               (IsString (fromString))
import Options.Applicative

import LambdaComp.Optimizations (Optimization (..), integerToOptimizationClass)
import Prettyprinter.Render.String (renderString)
import qualified Prettyprinter as Pretty

data Options where
  Options :: { input :: FilePath
             , backend :: Backend c
             , phase :: Phase c
             , optimizations :: Set Optimization
             , output :: FilePathFor c }
          -> Options

parseOptions :: IO Options
parseOptions = execParser progInfo

printHelpForError :: String -> IO a
printHelpForError h =
  handleParseResult
  . Failure
  $ parserFailure (prefs showHelpOnError) progInfo (ErrorMsg h) mempty

progInfo :: ParserInfo Options
progInfo =
  info (getOptions <**> commonParserOptions helper)
  $ progDesc "A primitive λ-calculus (+ α) compiler using CBPV"
  <> failureCode 1

getOptions :: Parser Options
getOptions =
  commonParserOptions getInputFilePath
  <**> ((getOptionsForDirectCBackend <|> getOptionsForAMBackend)
        <*> phaseParserOptions getCommonPhase
        <*> optimizationParserOptions getOptimizations)

getInputFilePath :: Parser FilePath
getInputFilePath = strArgument (metavar "INPUT_FILE" <> help "The path of an input file." <> action "file")

{-# ANN getOptionsForDirectCBackend ("HLint: ignore Avoid lambda" :: String) #-}
getOptionsForDirectCBackend :: Parser (Maybe AnyPhase -> Set Optimization -> FilePath -> Options)
getOptionsForDirectCBackend =
  (\backend phase fp mayPhase opts input ->
     Options input backend (maybe phase (\p -> getPhase p) mayPhase) opts fp)
  <$> backendParserOptions getDirectCBackend
  <*> phaseParserOptions getDirectCPhase
  <*> commonParserOptions (optional getOutputFilePath)

getDirectCBackend :: Parser (Backend DirectCBackendType)
getDirectCBackend =
  flag' DirectCBackend (long "direct-c"
                        <> short 'c'
                        <> help "Use the backend that goes directly to C.")

getDirectCPhase :: Parser (Phase DirectCBackendType)
getDirectCPhase =
  asum
  [ getHiddenTargetPhase [UntilC, UntilExe]
  , pure UntilExe
  ]

{-# ANN getOptionsForAMBackend ("HLint: ignore Avoid lambda" :: String) #-}
getOptionsForAMBackend :: Parser (Maybe AnyPhase -> Set Optimization -> FilePath -> Options)
getOptionsForAMBackend =
  (\backend phase mayPhase opts input ->
     Options input backend (maybe phase (\p -> getPhase p) mayPhase) opts ())
  <$> backendParserOptions getAMBackend
  <*> phaseParserOptions getAMPhase

getAMBackend :: Parser (Backend AMBackendType)
getAMBackend =
  asum
  [ flag' AMBackend (long "am"
                     <> help "Use the backend that goes through an abstract machine (AM). This is the default.")
  , pure AMBackend
  ]

getAMPhase :: Parser (Phase AMBackendType)
getAMPhase =
  asum
  [ getHiddenTargetPhase [UntilAM]
  , pure UntilAM
  ]

data AnyPhase where
  AnyPhase :: { getPhase :: forall c. Phase c } -> AnyPhase

getCommonPhase :: Parser (Maybe AnyPhase)
getCommonPhase =
  optional
  (getHiddenTargetAnyPhase [AnyPhase UntilAST, AnyPhase UntilElaboration, AnyPhase UntilElaborationOpt, AnyPhase UntilCBPV, AnyPhase UntilCBPVOpt]
   <|> flag' (AnyPhase Run) (long (getPhaseOption Run)
                             <> short 'r'
                             <> help (getPhaseOptionHelp Run)))

getHiddenTargetPhase :: [Phase c] -> Parser (Phase c)
getHiddenTargetPhase =
  asum
  . map
    (\p ->
       flag' p (long (getPhaseOption p)
                <> hidden
                <> help (getPhaseOptionHelp p)))

getHiddenTargetAnyPhase :: [AnyPhase] -> Parser AnyPhase
getHiddenTargetAnyPhase =
  asum
  . map
    (\p ->
       flag' p (long (getPhaseOption $ getPhase p)
                <> hidden
                <> help (getPhaseOptionHelp $ getPhase p)))

getOptimizations :: Parser (Set Optimization)
getOptimizations = getOptimizationClass <**> getOptimizationMap

getOptimizationClass :: Parser (Set Optimization)
getOptimizationClass =
  integerToOptimizationClass <$> (option auto (short 'O') <|> pure 1)

getOptimizationMap :: Parser (Set Optimization -> Set Optimization)
getOptimizationMap = foldr (.) id <$> many getOneOptimizationMap
  where
    getOneOptimizationMap =
      asum
      . map
        (\opt ->
           let
             (name, onOptName, offOptName) = getOptimizationNameAndOptions opt
           in
             asum
             [ flag' (<> Set.singleton opt) (long onOptName
                                             <> hidden
                                             <> help ("Turn on the optimization " <> name <> "."))
             , flag' (opt `Set.delete`) (long offOptName
                                         <> hidden)
             ])
      $ [ OElabConstantPropagation
        , OCBPVBetaReduction
        , OCBPVCommutingIf
        , OCBPVCommutingPrint
        , OCBPVCommutingTo
        , OCBPVDeadLetElimination
        , OCBPVEtaReduction
        , OCBPVInlineSimpleLet
        , OCBPVInlineLinearLet
        , OCBPVLiftingLet
        ]

getOutputFilePath :: Parser FilePath
getOutputFilePath =
  strOption
  $ long "output"
  <> short 'o'
  <> metavar "OUTPUT_FILE"
  <> action "file"
  <> help "Set the output path to OUTPUT_FILE. Useful for some phases."

type data BackendType where
  DirectCBackendType, AMBackendType :: BackendType

type role Backend nominal
data Backend (c :: BackendType) where
  DirectCBackend :: Backend DirectCBackendType
  AMBackend      :: Backend AMBackendType

type role Phase nominal
data Phase (c :: BackendType) where
  UntilAST,
    UntilElaboration,
    UntilElaborationOpt,
    UntilCBPV,
    UntilCBPVOpt   :: Phase c
  UntilC, UntilExe :: Phase DirectCBackendType
  UntilAM          :: Phase AMBackendType
  Run              :: Phase c

getPhaseOption :: Phase c -> String
getPhaseOption UntilAST            = "until-ast"
getPhaseOption UntilElaboration    = "until-elab"
getPhaseOption UntilElaborationOpt = "until-elab-opt"
getPhaseOption UntilCBPV           = "until-cbpv"
getPhaseOption UntilCBPVOpt        = "until-cbpv-opt"
getPhaseOption UntilC              = "until-c"
getPhaseOption UntilExe            = "until-exe"
getPhaseOption UntilAM             = "until-am"
getPhaseOption Run                 = "run"

getPhaseOptionHelp :: Phase c -> String
getPhaseOptionHelp UntilAST            = "Stop after parsing an AST and print it."
getPhaseOptionHelp UntilElaboration    = "Stop after generating an elaborated AST and print it."
getPhaseOptionHelp UntilElaborationOpt = "Stop after generating an optimized AST and print it."
getPhaseOptionHelp UntilCBPV           = "Stop after generating a CBPV term and print it."
getPhaseOptionHelp UntilCBPVOpt        = "Stop after optimizing a CBPV term and print it."
getPhaseOptionHelp UntilC              = "Stop after generating a C and write that to the given output path. Available only for Direct-C backend."
getPhaseOptionHelp UntilExe            = "Stop after generating an executable using C, which is written to the given output path. Currently available only for the direct-c backend. This is the default for the Direct-C backend."
getPhaseOptionHelp UntilAM             = "Stop after generating an abstract machine (AM) term and print it. Available only for the AM backend. This is the default for the AM backend."
getPhaseOptionHelp Run                 = "Run the example term using the given backend. For the Direct-C backend, this generates and executes an executable using C, which is written to the given output path. For the AM backend, this interprets an abstract machine (AM) term and prints the result value."

showPhaseOption :: Phase c -> String
showPhaseOption p = "--" <> getPhaseOption p <> " option"

getOptimizationNameAndOptions :: Optimization -> (String, String, String)
getOptimizationNameAndOptions opt = (kebabOptText, "opt-" <> kebabOptText, "opt-no-" <> kebabOptText)
  where
    kebabOptText
      | Just text <- stripPrefix "OElab" optText = "elab" <> pascalToKebabCase text
      | Just text <- stripPrefix "OCBPV" optText = "cbpv" <> pascalToKebabCase text
      | otherwise                                = error "!!!!! Compiler Error !!!!!\nOptimization constructors are not covered in the option parser."

    optText = show opt

    pascalToKebabCase []       = []
    pascalToKebabCase (x : xs)
      | isUpperCase x          = '-' : toLower x : pascalToKebabCase xs
      | otherwise              = x : pascalToKebabCase xs

type family FilePathFor (c :: BackendType) = r | r -> c where
  FilePathFor DirectCBackendType = Maybe FilePath
  FilePathFor AMBackendType      = ()

commonParserOptions :: Parser a -> Parser a
commonParserOptions = parserOptionGroup "Common Options:"

backendParserOptions :: Parser a -> Parser a
backendParserOptions = parserOptionGroup "Available Backends:"

phaseParserOptions :: Parser a -> Parser a
phaseParserOptions = parserOptionGroup "Available Phases:"

optimizationParserOptions :: Parser a -> Parser a
optimizationParserOptions =
  parserOptionGroup
  . renderString
  . Pretty.layoutSmart Pretty.defaultLayoutOptions
  $ Pretty.vsep
    [ "Options for Optimizations:"
    , Pretty.indent 2
      $ Pretty.vsep
        [ "One can disable an optimization putting 'no-' prefix after 'opt-'."
        , "For example, " <> fromString noOptName <> " disables " <> fromString name <> "."
        ]
    , Pretty.emptyDoc
    ]
  where
    (name, _, noOptName) = getOptimizationNameAndOptions OElabConstantPropagation
