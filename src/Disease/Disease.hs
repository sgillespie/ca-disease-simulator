module Disease.Disease where

import Control.Comonad
import System.Random hiding (split)

import Disease.Universe

-- Be careful with the order here, we expect
-- Alive and Immune to be 1st and 2nd below
data DiseaseCell = Alive
                 | Immune
                 | Infected
                 | NotUsed -- For cells that "neutral cells"
                 deriving (Show, Enum, Eq)

-- |Comonad transition rule for a disease cell
--  Rule:
--   * If an Alive cell is adjacent to Infected, it becomes Infected
--   * Otherwise there is no state change
diseaseRule :: Universe DiseaseCell -> DiseaseCell
diseaseRule u | extract u == Alive && any (==Infected) (neighbors u) = Infected
              | otherwise = extract u

diseaseRule2 :: Universe2 DiseaseCell -> DiseaseCell
diseaseRule2 u | extract u == Alive && any (==Infected) (neighbors2 u) = Infected
               | otherwise = extract u

-- |Generate a random disease, using these rules:
--
--  1) Exactly 1 infected cell
--  2) Every other cell is either Alive or Immune (50/50)
genDisease :: RandomGen g => Int -> g -> Universe DiseaseCell
genDisease size gen = fromList (take size cells) NotUsed
  where (infectedPos, gen') = randomR (0, size-1) gen
        (c1, c2)            = splitAt infectedPos (randomCells gen')
        cells               = c1 ++ [Infected] ++ c2
        
randomCells :: RandomGen g => g -> [DiseaseCell]
randomCells = map toEnum . randomRs (0, 1)

-- |Generate a random disease of Universe2, using the same rules
--  as randomDisease
genDisease2 :: RandomGen g => Int -> Int -> g -> Universe2 DiseaseCell
genDisease2 cols rows gen = fromList2 splitted NotUsed
  where (infectedPosX, gen')  = randomR (0, cols-1) gen
        (infectedPosY, gen'') = randomR (0, rows-1) gen'
        cells = randomCells gen''
        splitted = split cols rows infectedPosX infectedPosY cells
        

split :: Int -> Int -> Int -> Int -> [DiseaseCell] -> [[DiseaseCell]]
split _ 0 _ _ _ = []
split cols rows infectedPosX 0 cells = row : (split cols (rows-1) infectedPosX (-1) cells'')
  where (rs, cells')   = splitAt infectedPosX cells
        (rss, cells'') = splitAt (cols-1-infectedPosX) cells'
        row = rs ++ [Infected] ++ rss
split cols rows infectedPosX infectedPosY cells = row : (split cols (rows-1) infectedPosX (infectedPosY-1) cells')
  where (row, cells') = splitAt cols cells
