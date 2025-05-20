module Main
  ( main
  ) where

import System.Exit (exitWith)
import System.IO   (stdout)

import LambdaComp.Driver          (mainFuncWithOptions)
import LambdaComp.Driver.Argument (parseOptions)

main :: IO ()
main = parseOptions >>= mainFuncWithOptions stdout >>= exitWith
