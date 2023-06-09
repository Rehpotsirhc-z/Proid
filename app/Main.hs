module Main (main) where

import Lib (handle)
import System.Directory (getTemporaryDirectory)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  tmpDir <- getTemporaryDirectory
  handle tmpDir args
