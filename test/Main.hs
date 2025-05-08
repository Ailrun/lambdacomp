module Main (main) where

import Control.Monad        (void)
import Data.ByteString.Lazy qualified as LBS
import System.Directory     (makeAbsolute)
import System.FilePath      (takeFileName, (<.>), (</>))
import System.IO            (hClose)
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

    [ testGroup "compile"
      [ cCompileTests allExamples
      , amCompileTests allExamples
      ]

    , testGroup "execution"
      [ cExecutionTests allExamples
      , amExecutionTests allExamples
      ]
    ]
  ]

cCompileTests :: [FilePath] -> TestTree
cCompileTests allExamples =
  testGroup "C backend"
  $ compileOfExample "c" (makeCOptions UntilExe) <$> allExamples

amCompileTests :: [FilePath] -> TestTree
amCompileTests allExamples =
  testGroup "AM backend (interpreter)"
  $ compileOfExample "am" (makeAMOptions UntilAM) <$> allExamples

compileOfExample :: String -> (FilePath -> Options) -> String -> TestTree
compileOfExample tag optionBuilder s =
  goldenVsStringDiff s gitDiff ("." </> "test" </> "golden" </> s <.> tag <.> "compile")
  $ withSystemTempFile s
  $ \fp handle -> do
    getExamplePath s
      >>= void
      . mainFuncWithOptions handle
      . optionBuilder
    hClose handle
    LBS.take 10000 <$> LBS.readFile fp

cExecutionTests :: [FilePath] -> TestTree
cExecutionTests allExamples =
  testGroup "C backend"
  $ executionOfExample "c" (makeCOptions Run) <$> allExamples

amExecutionTests :: [FilePath] -> TestTree
amExecutionTests allExamples =
  testGroup "AM backend (interpreter)"
  $ executionOfExample "am" (makeAMOptions Run) <$> allExamples

executionOfExample :: String -> (FilePath -> Options) -> String -> TestTree
executionOfExample tag optionBuilder s =
  goldenVsStringDiff s gitDiff ("." </> "test" </> "golden" </> s <.> tag <.> "execution")
  $ withSystemTempFile s
  $ \fp handle -> do
    getExamplePath s
      >>= void
      . timeout 300000
      . mainFuncWithOptions handle
      . optionBuilder
    hClose handle
    LBS.take 10000 <$> LBS.readFile fp

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
