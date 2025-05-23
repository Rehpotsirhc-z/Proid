-- [[file:../proid.org::*License][License:1]]
-- SPDX-FileCopyrightText: 2025 Rehpotsirhc
--
-- SPDX-License-Identifier: GPL-3.0-or-later
-- License:1 ends here

-- [[file:../proid.org::*Imports][Imports:1]]
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.String.QQ (s)
import System.Console.ANSI
  ( Color (Red),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGRCode,
  )
import System.Directory
  ( doesFileExist,
    getTemporaryDirectory,
    renameFile,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error
  ( catchIOError,
    isDoesNotExistError,
    isPermissionError,
  )
import System.IO.Temp (writeSystemTempFile)
import System.Process (callProcess, readProcess)
-- Imports:1 ends here

-- [[file:../proid.org::*Data Types][Data Types:1]]
data ProidMode = Read | Write deriving (Show, Read, Eq)
-- Data Types:1 ends here

-- [[file:../proid.org::*Main][Main:1]]
main :: IO ()
main = do
  args <- getArgs
  tmpDir <- getTemporaryDirectory
  handleArgs tmpDir args
-- Main:1 ends here

-- [[file:../proid.org::*Arguments][Arguments:1]]
handleArgs :: FilePath -> [String] -> IO ()
handleArgs _ [] = do
  printError "No arguments provided"
  hPutStrLn stderr "Try --help for more information"
  exitFailure
handleArgs _ ["--help"] = printHelp
handleArgs _ ["-h"] = printHelp
handleArgs tmpDir [action] = dispatch tmpDir (lookup action actionMap)
handleArgs _ (_ : _) = do
  printError "You can only specify one argument"
  hPutStrLn stderr "Try --help for more information"
  exitFailure
-- Arguments:1 ends here

-- [[file:../proid.org::*Arguments][Arguments:2]]
actionMap :: [(String, (FilePath -> FilePath -> IO (), String))]
actionMap =
  [ ("hide", (proidHide, "proidlog")),
    ("show", (proidShow, "proidlog")),
    ("deshide", (proidHide, "desproidlog")),
    ("desshow", (proidShow, "desproidlog"))
  ]

dispatch :: t1 -> Maybe (t1 -> t2 -> IO b, t2) -> IO b
dispatch _ Nothing = do
  printError "Invalid argument"
  hPutStrLn stderr "Try --help for more information"
  exitFailure
dispatch tmpDir (Just (action, filename)) = action tmpDir filename
-- Arguments:2 ends here

-- [[file:../proid.org::*Hide][Hide:1]]
proidHide :: FilePath -> FilePath -> IO ()
proidHide tmpDir filename = do
  proid <-
    readProcess "xdotool" ["getactivewindow"] []
      `catchIOError` xdotoolError
  writeToFile (tmpDir </> filename) proid
  callProcess "xdotool" ["windowunmap", init proid]
    `catchIOError` xdotoolError
-- Hide:1 ends here

-- [[file:../proid.org::*Show][Show:1]]
proidShow :: FilePath -> FilePath -> IO ()
proidShow tmpDir filename = do
  proid <- eraseFromFile (tmpDir </> filename)
  callProcess "xdotool" ["windowmap", proid]
    `catchIOError` xdotoolError
-- Show:1 ends here

-- [[file:../proid.org::*Write][Write:1]]
writeToFile :: FilePath -> String -> IO ()
writeToFile filename string =
  catchIOError
    ( do
        exists <- doesFileExist filename
        if exists
          then appendFile filename string
          else writeFile filename string
    )
    ( \e ->
        printError (handleLogError Write filename "Couldn't access temporary directory" e)
          >> exitFailure
    )
-- Write:1 ends here

-- [[file:../proid.org::*Erase][Erase:1]]
eraseFromFile :: FilePath -> IO String
eraseFromFile path = do
  let filename = takeFileName path

  text <-
    readFile path
      `catchIOError` ( \e ->
                         printError (handleLogError Read path "No window to show" e)
                           >> exitFailure
                     )

  if null text
    then do
      printError "No window to show"
      exitFailure
    else do
      let list = lines text
      let proid = last list
      tmp <-
        writeSystemTempFile filename (intercalate "\n" (init list))
          `catchIOError` (\e -> printError (handleLogError Write "temporary file" "" e) >> exitFailure)
      renameFile tmp path
        `catchIOError` (\e -> printError (handleLogError Write path "" e) >> exitFailure)
      return proid
-- Erase:1 ends here

-- [[file:../proid.org::*Error Messages][Error Messages:1]]
printError :: String -> IO ()
printError str =
  hPutStrLn stderr $
    setSGRCode [SetColor Foreground Vivid Red]
      ++ str
      ++ setSGRCode [Reset]
-- Error Messages:1 ends here

-- [[file:../proid.org::*Errors][Errors:1]]
handleLogError :: ProidMode -> String -> String -> IOError -> String
handleLogError action file message e
  | isDoesNotExistError e = message
  | isPermissionError e =
      "Couldn't "
        ++ map toLower (show action)
        ++ word
        ++ file
        ++ " due to lack of permissions"
  | otherwise =
      "Couldn't "
        ++ map toLower (show action)
        ++ word
        ++ file
  where
    word = if action == Read then " from " else " to "

xdotoolError :: p -> IO b
xdotoolError _ = do
  printError "Failed to run xdotool (maybe it is not installed)"
  exitFailure
-- Errors:1 ends here

-- [[file:../proid.org::*Help][Help:1]]
printHelp :: IO ()
printHelp = putStrLn helpMessage
  where
    helpMessage =
      [s|
Usage: proid [OPTION]
Hides and shows windows

Options:
  hide        Hide the current window
  show        Show the most recently hidden window
  deshide     Hide the current window with a priority
  desshow     Show the most recently hidden window with a priority
  --help      Show this help message

Examples:
  proid hide
  proid show
  proid deshide|]
-- Help:1 ends here
