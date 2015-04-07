module Disease.Disease where

import System.Random

import Disease.Universe

data DiseaseCell = Alive
                 | Immune
                 | Infected
                 | NotUsed -- For cells that "neutral cells"
                 deriving (Show, Enum, Eq)

-- TODO[sgillespie]: Generate a random universe

