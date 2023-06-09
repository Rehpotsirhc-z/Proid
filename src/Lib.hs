{-# LANGUAGE QuasiQuotes #-}

module Lib (handle) where

import Data.List (intercalate)
import Data.String.QQ (s)
import System.Console.ANSI
  ( Color (Red),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGRCode,
  )
import System.Directory (doesFileExist, renameFile)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (writeSystemTempFile)
import System.Process (callProcess, readProcess)

printError :: [Char] -> IO ()
printError str = hPutStrLn stderr $ setSGRCode [SetColor Foreground Vivid Red] ++ str ++ setSGRCode [Reset]

handle :: FilePath -> [String] -> IO ()
handle _ [] =
  printError "No arguments provided"
    >> hPutStrLn stderr "Try --help for more information"
    >> exitFailure
handle _ ["--help"] = printHelp
handle _ ["-h"] = printHelp
handle tmpDir ["hide"] = proidHide tmpDir "proidlog"
handle tmpDir ["show"] = proidShow tmpDir "proidlog"
handle tmpDir ["deshide"] = proidHide tmpDir "desproidlog"
handle tmpDir ["desshow"] = proidShow tmpDir "desproidlog"
handle _ [_] =
  printError "Invalid argument"
    >> hPutStrLn stderr "Try --help for more information"
    >> exitFailure
handle _ (_ : _) =
  printError "You can only specify one argument"
    >> hPutStrLn stderr "Try --help for more information"
    >> exitFailure

printHelp :: IO ()
printHelp = do
  let helpMessage :: String
      helpMessage =
        [s|
Usage: proid [OPTION]
Hides and shows windows

Options:
  hide        Hide the current window
  show        Show the most recently hidden window
  deshide     Hide the current window with a priority
  desshow     Show the most recently hiddne window with a priority
  --help      Show this help message

Examples:
  proid hide
  proid show
  proid deshide|]
  putStrLn helpMessage

proidHide :: FilePath -> FilePath -> IO ()
proidHide tmpDir filename = do
  proid <- readProcess "xdotool" ["getactivewindow"] []
  writeToFile (tmpDir </> filename) proid
  callProcess "xdotool" ["windowunmap", init proid]

proidShow :: FilePath -> FilePath -> IO ()
proidShow tmpDir filename = do
  proid <- eraseFromFile (tmpDir </> filename)
  callProcess "xdotool" ["windowmap", proid]

writeToFile :: FilePath -> String -> IO ()
writeToFile filename string = do
  exists <- doesFileExist filename
  if exists
    then appendFile filename string
    else writeFile filename string

eraseFromFile :: FilePath -> IO String
eraseFromFile path = do
  let filename = takeFileName path
  exists <- doesFileExist path

  if exists
    then do
      text <- readFile path
      if not $ null text
        then do
          let list = lines text
          let proid = last list

          tmp <- writeSystemTempFile filename (intercalate "\n" (init list))

          renameFile tmp path
          return proid
        else printError "No window to show" >> exitFailure
    else printError "No window to show" >> exitFailure
