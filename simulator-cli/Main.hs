module Main where

import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random

import Disease.Disease
import Disease.Universe

data DiseaseSettings = DiseaseSettings
  { interactive :: Bool,        -- Whether to automatically update
    rows :: Int,                -- Rows of CA
    cols :: Int,                -- Columns of CA
    percentImmune :: Float,     -- Probably of each cell being immune
    updateInterval :: Int,      -- In milliseconds
    help :: Bool                -- Show usage?
  }
  deriving (Eq, Show)

defaultSettings = DiseaseSettings
  { interactive = False,
    rows = 50,
    cols = 50,
    percentImmune = 0.5,
    updateInterval = 1000,
    help = False
  }

data CmdFlag
  = Columns
  | Help
  | Interactive
  | PercentImmune
  | Rows
    deriving (Eq, Show)

options :: [OptDescr (DiseaseSettings -> DiseaseSettings)]
options
  = [Option ['i'] ["interactive"] (NoArg (\opts -> opts { interactive = True })) "Only update state after a pressing \"enter\"",
     Option ['r'] ["rows"] (ReqArg (\a opts -> opts { rows = read a }) "rows") "Specify the number of rows to generate",
     Option ['c'] ["columns"] (ReqArg (\a opts -> opts { cols = read a }) "columns") "Specify the number of columns to generate",
     Option ['p'] ["immune"] (ReqArg (\a opts -> opts { percentImmune = read a }) "percent") "The probability a cell has of being immune (default: 0.5)",
     Option ['h'] ["help"] (NoArg (\opts -> opts {help = True})) "Show this message"]

parseArgs :: [String] -> IO DiseaseSettings
parseArgs argv = case (getOpt (Permute) options argv) of
  (opts, [], [])   -> return (foldl (flip id) defaultSettings opts)
  (_,    _,  errs) -> printUsageAndExit errs

usageHeader :: String
usageHeader = "Usage: sim-disease [options]"

printUsageAndExit :: [String] -> IO a
printUsageAndExit errors = do
  mapM_ (hPutStr stderr) errors
  hPutStrLn stderr ""
  hPutStrLn stderr (usageInfo usageHeader options)
  exitFailure

maybePrintUsageAndExit :: [String] -> DiseaseSettings -> IO ()
maybePrintUsageAndExit (x:xs) _ = printUsageAndExit (x:xs)
maybePrintUsageAndExit _      (DiseaseSettings {help=True}) = printUsageAndExit []
maybePrintUsageAndExit _      _ = return ()


newDisease :: DiseaseSettings -> IO (Universe2 DiseaseCell)
newDisease (DiseaseSettings {rows=rows, cols=cols})
  = liftM (genDisease2 rows cols) newStdGen

main :: IO ()
main = do
  flags <- getArgs >>= parseArgs
  maybePrintUsageAndExit [] flags
  disease <- newDisease flags
  
  return ()
