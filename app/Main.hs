module Main (main) where

import Data.List (intercalate)
import System.Console.ANSI
  ( Color (Red),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGRCode,
  )
import System.Directory (doesFileExist, getHomeDirectory, renameFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (writeTempFile)
import System.Process (callProcess, readProcess)

main :: IO ()
main = do
  args <- getArgs
  handle args

printError :: [Char] -> IO ()
printError str = hPutStrLn stderr $ setSGRCode [SetColor Foreground Vivid Red] ++ str ++ setSGRCode [Reset]

handle :: [String] -> IO ()
handle [] = printError "No arguments provided" >> hPutStrLn stderr "Try --help for more information" >> exitFailure
handle ["--help"] = printHelp
handle ["-h"] = printHelp
handle ["hide"] = proidHide ".proidlog"
handle ["show"] = proidShow ".proidlog"
handle ["deshide"] = proidHide ".desproidlog"
handle ["desshow"] = proidShow ".desproidlog"
handle [_] = printError "Invalid argument" >> hPutStrLn stderr "Try --help for more information" >> exitFailure
handle (_ : _) = printError "You can only specify one argument" >> hPutStrLn stderr "Try --help for more information" >> exitFailure

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: proid [OPTION]"
  putStrLn "Hides and shows windows"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  hide        Hide the current window"
  putStrLn "  show        Show the most recently hidden window"
  putStrLn "  deshide     Hide the current window with a priority"
  putStrLn "  desshow     Show the most recently hiddne window with a priority"
  putStrLn "  --help      Show this help message"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  proid hide"
  putStrLn "  proid show"
  putStrLn "  proid deshide"

proidHide :: FilePath -> IO ()
proidHide filename = do
  proid <- readProcess "xdotool" ["getactivewindow"] []
  home <- getHomeDirectory
  writeToFile (home ++ "/.config/" ++ filename) proid
  callProcess "xdotool" ["windowunmap", init proid]

proidShow :: FilePath -> IO ()
proidShow filename = do
  home <- getHomeDirectory
  proid <- eraseFromFile (home ++ "/.config/") filename
  callProcess "xdotool" ["windowmap", proid]

writeToFile :: FilePath -> String -> IO ()
writeToFile filename string = do
  exists <- doesFileExist filename
  if exists
    then appendFile filename string
    else writeFile filename string

eraseFromFile :: FilePath -> FilePath -> IO String
eraseFromFile directory filename = do
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
        else printError "No window to show" >> exitFailure
    else printError "No window to show" >> exitFailure
