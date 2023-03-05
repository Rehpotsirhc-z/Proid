module Main (main) where

-- import Lib

import Data.List (intercalate)
import System.Directory (doesFileExist, getHomeDirectory, renameFile)
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (writeTempFile)
import System.Process (callProcess, readProcess)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then hPutStrLn stderr "No arguments provided!"
    else handle (head args)

handle :: String -> IO ()
handle action = case action of
  "hide" -> proidhide ".proidlog"
  "show" -> proidshow ".proidlog"
  "desshow" -> proidshow ".desproidlog"
  "deshide" -> proidhide ".desproidlog"
  _ -> hPutStrLn stderr "No valid arguments"

proidhide :: [Char] -> IO ()
proidhide filename = do
  proid <- readProcess "xdo" ["id"] []
  home <- getHomeDirectory
  write (home ++ "/.config/" ++ filename) proid
  callProcess "xdo" ["hide", init proid]

proidshow :: [Char] -> IO ()
proidshow filename = do
  home <- getHomeDirectory
  proid <- erase (home ++ "/.config/") filename
  callProcess "xdo" ["show", proid]

write :: FilePath -> String -> IO ()
write filename string = do
  exists <- doesFileExist filename
  if exists
    then appendFile filename string
    else writeFile filename string

erase :: [Char] -> [Char] -> IO String
erase directory filename = do
  -- exists <- doesFileExist filename
  -- if exists
  --   then appendFile filename string
  --   else writeFile filename string
  let path = directory ++ filename
  exists <- doesFileExist path
  if exists
    then do
      text <- readFile path
      if not $ null text
        then do
          let list = lines text
          let proid = last list
          tmp <- writeTempFile directory filename (intercalate "\n" (init list))
          renameFile tmp path
          return proid
        else do
          hPutStrLn stderr "No window to show"
          exitFailure
    else do
      hPutStrLn stderr "No window to show"
      exitFailure
