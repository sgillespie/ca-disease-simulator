module Main where

import Control.Concurrent
import Control.Comonad
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Random
import Text.Printf

import Disease.Disease
import Disease.Universe
import Disease.Vaccine

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
    updateInterval = 1000000,
    help = False
  }

options :: [OptDescr (DiseaseSettings -> DiseaseSettings)]
options
  = [Option ['i'] ["interactive"] (NoArg (\opts -> opts { interactive = True })) "Only update state after a pressing \"enter\"",
     Option ['r'] ["rows"] (ReqArg (\a opts -> opts { rows = read a }) "rows") "Specify the number of rows to generate",
     Option ['c'] ["columns"] (ReqArg (\a opts -> opts { cols = read a }) "columns") "Specify the number of columns to generate",
     Option ['p'] ["immune"] (ReqArg (\a opts -> opts { percentImmune = read a }) "percent")
                             "The probability a cell has of being immune (default: 0.5)",
     Option ['u'] ["update-interval"] (ReqArg (\a opts -> opts { updateInterval = (read a) * 1000000 }) "interval")
                                      "The update interval of the grid in seconds (default 1)",
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
newDisease (DiseaseSettings {rows=rows, cols=cols, percentImmune=immune})
  = liftM (genDisease2 immune rows cols) newStdGen

renderDisease :: Universe2 DiseaseCell -> String
renderDisease = unlines . map concat . map (map renderCell) . toList2
  where renderCell Alive = "\x1b[43m "
        renderCell Immune = "\x1b[42m "
        renderCell Infected = "\x1b[41m "
        renderCell NotUsed = "\x1b[40m "

main :: IO ()
main = do
  flags <- getArgs >>= parseArgs
  maybePrintUsageAndExit [] flags
  disease <- newDisease flags
  gameLoop 0 flags disease

gameLoop :: Int -> DiseaseSettings -> Universe2 DiseaseCell -> IO ()
gameLoop iter settings u = do
  rawSystem "clear" []
  putStrLn . renderDisease $ u
  if (interactive settings)
    then putStr "\x1b[0mPress [enter]: " >> hFlush stdout >> getLine >> return ()
    else threadDelay (updateInterval settings)
  u' <- if (iter > 30) then return (vaccinate2 u) else return u
  if (u =>> diseaseRule2) == u
    then renderStats u
    else gameLoop (iter+1) settings (u' =>> diseaseRule2)
  
renderStats :: Universe2 DiseaseCell -> IO ()
renderStats u = do
  let totalCells = sum . map length . toList2 $ u
      aliveCells = sum . map length . map (filter ((flip elem) [Alive, Immune])) . toList2 $ u
      deadCells = sum . map length . map (filter ((flip elem) [Infected])) . toList2 $ u
      savedCells = sum . map length . map (filter (==Alive)) . toList2 $ u
  
  putStrLn "Final Statistics"
  putStrLn $ printf "Total Cells: %v (100%%)" totalCells
  putStrLn $ printf "Total Alive: %v (%v%%)" aliveCells (percent aliveCells totalCells)
  putStrLn $ printf "Total Dead: %v (%v%%)" deadCells (percent deadCells totalCells)
  putStrLn $ printf "Total Alive (not vaccinated): %v (%v%%)" savedCells (percent savedCells totalCells)

percent :: Int -> Int -> Int
percent v1 v2 = floor $ (fromIntegral v1) / (fromIntegral v2) * 100
