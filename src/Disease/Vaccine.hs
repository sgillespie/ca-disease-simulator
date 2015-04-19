module Disease.Vaccine where

import Control.Comonad

import Disease.Disease
import Disease.Universe

-- |Vaccinate one cell. Our strategy will be to find a cell
-- that is between two immune cells.  We attempt to contain
-- the disease by "closing" gaps betwen immune cells
--
-- * Start some where away from the disease TODO[sgillespie]: Start at middle or random?
-- * Look for vaccination candidates nearby
vaccinate :: Universe DiseaseCell -> Universe DiseaseCell
vaccinate u = case closestAlive u of
    Nothing -> u
    Just (Universe s o ls x rs) -> Universe s o ls Immune rs

closestAlive :: Universe DiseaseCell -> Maybe (Universe DiseaseCell)
closestAlive u | extract (closestAlive' (left', right')) == NotUsed = Nothing -- Can't find one
               | otherwise = Just . closestAlive' $ (left', right')
  where (left', right') = closest (flip elem $ [Alive, NotUsed]) u

        closestAlive' :: (Universe DiseaseCell, Universe DiseaseCell) -> Universe DiseaseCell
        closestAlive' (l, r) | extract l == NotUsed = r
                             | extract r == NotUsed = l
                             | otherwise = if (offset u - offset l < offset r - offset u)
                                             then l
                                             else r

closest :: (DiseaseCell -> Bool) -> Universe DiseaseCell -> (Universe DiseaseCell, Universe DiseaseCell)
closest test u = (closestWith test left u, closestWith test right u)

closestWith :: (DiseaseCell -> Bool) -> (Universe DiseaseCell -> Universe DiseaseCell)
                                     -> Universe DiseaseCell
                                     -> Universe DiseaseCell
closestWith test f u | test (extract u) = u
                     | otherwise = closestWith test f (f u)


                                                  

  
