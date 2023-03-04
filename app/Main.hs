module Main (main) where

-- import Lib

import Data.List (intercalate)
import System.Directory (getHomeDirectory, renameFile)
import System.Environment (getArgs)
import System.IO (IOMode (AppendMode), hClose, hPutStr, hPutStrLn, openFile, stderr)
import System.IO.Temp (writeTempFile)
import System.Process (readProcess)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then hPutStrLn stderr "No arguments provided"
    else handle (head args)

handle :: String -> IO ()
handle action = case action of
  "hide" -> proidhide
  "show" -> proidshow
  _ -> hPutStrLn stderr "No valid arguments"

proidhide :: IO ()
proidhide = do
  proid <- readProcess "xdo" ["id"] []
  home <- getHomeDirectory
  write (home ++ "/.config/.proidlog") proid

proidshow :: IO ()
proidshow = do
  home <- getHomeDirectory
  erase (home ++ "/.config/.proidlog")

write :: FilePath -> String -> IO ()
write filename string = do
  file <- openFile filename AppendMode
  hPutStr file string
  hClose file

erase :: FilePath -> IO ()
erase filename = do
  text <- readFile filename
  -- TODO: empty file
  let list = lines text
  let proid = last list
  tmp <- writeTempFile "/home/rehpotsirhc/.config" ".proidlog" (intercalate "\n" (init list))
  print proid
  renameFile tmp filename
