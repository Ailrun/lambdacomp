module Main (main) where

import Control.Monad        (void)
import Data.ByteString.Lazy qualified as LBS
import Data.Int             (Int64)
import System.Directory     (makeAbsolute)
import System.FilePath      (takeFileName, (<.>), (</>))
import System.IO            (Handle, hClose)
import System.IO.Temp       (withSystemTempFile)
import System.Timeout       (timeout)
import Test.Tasty
import Test.Tasty.Golden    (findByExtension, goldenVsStringDiff)

import LambdaComp.Driver          (mainFuncWithOptions)
import LambdaComp.Driver.Argument (Backend (..), BackendType (..), Options (..), Phase (..))

main :: IO ()
main = listAllExamples >>= defaultMain . tests

tests :: [FilePath] -> TestTree
tests allExamples =
  testGroup "λ-compiler tests"
  [ testGroup "examples"

    [ testGroup "cbpv"
      [ anyCBPVTests allExamples
      ]

    , testGroup "cbpv-opt"
      [ anyCBPVOptTests allExamples
      ]

    , testGroup "code-gen"
      [ cGenTests allExamples
      , amGenTests allExamples
      ]

    , testGroup "compile"
      [ cCompileTests allExamples
      ]

    , testGroup "execution"
      [ cExecutionTests allExamples
      , amExecutionTests allExamples
      ]
    ]
  ]

anyCBPVTests :: [FilePath] -> TestTree
anyCBPVTests allExamples =
  testGroup "Any backend"
  $ getCBPVOfExample (makeAMOptions UntilCBPV) <$> allExamples

getCBPVOfExample :: (FilePath -> Options) -> String -> TestTree
getCBPVOfExample = goldenOf Nothing "cbpv" mainFuncWithOptions

anyCBPVOptTests :: [FilePath] -> TestTree
anyCBPVOptTests allExamples =
  testGroup "Any backend"
  $ getCBPVOptOfExample (makeAMOptions UntilCBPVOpt) <$> allExamples

getCBPVOptOfExample :: (FilePath -> Options) -> String -> TestTree
getCBPVOptOfExample = goldenOf Nothing ("cbpv" <.> "opt") mainFuncWithOptions

cGenTests :: [FilePath] -> TestTree
cGenTests allExamples =
  testGroup "C backend"
  $ codeGenOfExample "c" (makeCOptions UntilC) <$> allExamples

amGenTests :: [FilePath] -> TestTree
amGenTests allExamples =
  testGroup "AM backend"
  $ codeGenOfExample "am" (makeAMOptions UntilAM) <$> allExamples

codeGenOfExample :: String -> (FilePath -> Options) -> String -> TestTree
codeGenOfExample tag = goldenOf Nothing (tag <.> "code" <.> "gen") mainFuncWithOptions

cCompileTests :: [FilePath] -> TestTree
cCompileTests allExamples =
  testGroup "C backend"
  $ compileOfExample "c" (makeCOptions UntilExe) <$> allExamples

compileOfExample :: String -> (FilePath -> Options) -> String -> TestTree
compileOfExample tag = goldenOf Nothing (tag <.> "compile") mainFuncWithOptions

cExecutionTests :: [FilePath] -> TestTree
cExecutionTests allExamples =
  testGroup "C backend"
  $ executionOfExample "c" (makeCOptions Run) <$> allExamples

amExecutionTests :: [FilePath] -> TestTree
amExecutionTests allExamples =
  testGroup "AM backend (interpreter)"
  $ executionOfExample "am" (makeAMOptions Run) <$> allExamples

executionOfExample :: String -> (FilePath -> Options) -> String -> TestTree
executionOfExample tag = goldenOf (Just 10000) (tag <.> "execution") $ \handle ->
  timeout 300000 . mainFuncWithOptions handle

goldenOf :: Maybe Int64 -> String -> (Handle -> Options -> IO a) -> (FilePath -> Options) -> String -> TestTree
goldenOf maySize tag f optionBuilder s =
  goldenVsStringDiff s gitDiff ("." </> "test" </> "golden" </> s <.> tag)
  $ withSystemTempFile s
  $ \fp handle -> do
    getExamplePath s >>= void . f handle . optionBuilder
    hClose handle
    case maySize of
      Just size -> LBS.take size <$> LBS.readFile fp
      Nothing   -> LBS.readFile fp

makeCOptions :: Phase DirectCBackendType -> FilePath -> Options
makeCOptions phase input = Options { input, backend = DirectCBackend, phase, output = Nothing }

makeAMOptions :: Phase AMBackendType -> FilePath -> Options
makeAMOptions phase input = Options { input, backend = AMBackend, phase, output = () }

listAllExamples :: IO [String]
listAllExamples = getExampleDir >>= fmap (fmap takeFileName) . findByExtension [".lc"]

getExamplePath :: String -> IO FilePath
getExamplePath s = (</> s) <$> getExampleDir

getExampleDir :: IO FilePath
getExampleDir = makeAbsolute "examples"

gitDiff :: FilePath -> FilePath -> [String]
gitDiff ref new = ["git", "diff", ref, new]
