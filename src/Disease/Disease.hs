module Disease.Disease where

import System.Random

import Disease.Universe

-- Be careful with the order here, we expect
-- Alive and Immune to be 1st and 2nd below
data DiseaseCell = Alive
                 | Immune
                 | Infected
                 | NotUsed -- For cells that "neutral cells"
                 deriving (Show, Enum, Eq)

-- | Generate a random disease, using these rules:
--
--  1) Exactly 1 infected cell
--  2) Every other cell is either Alive or Immune (50/50)
randomDisease :: RandomGen g => Int -> g -> Universe DiseaseCell
randomDisease size gen = fromList (take size cells) NotUsed
  where (infectedPos, gen') = randomR (0, size-1) gen
        (c1, c2)            = splitAt infectedPos (randomCells gen')
        cells               = c1 ++ [Infected] ++ c2
        
randomCells :: RandomGen g => g -> [DiseaseCell]
randomCells = map toEnum . randomRs (0, 1)
