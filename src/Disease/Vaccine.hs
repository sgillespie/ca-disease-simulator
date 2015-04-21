module Disease.Vaccine where

import Control.Comonad

import Disease.Disease
import Disease.Universe

-- |Vaccinate one cell. We choose an alive cell close to the
-- current position
vaccinate :: Universe DiseaseCell -> Universe DiseaseCell
vaccinate u = case closestAlive u of
    Nothing -> u
    Just (Universe s o ls x rs) -> Universe s o ls Immune rs

-- |Vaccinate one cell. Our strategy will be to find a cell
-- that is between two immune cells.  We attempt to contain
-- the disease by "closing" gaps betwen immune cells
--
-- * Start some where away from the disease TODO[sgillespie]: Start at middle or random?
-- * Look for vaccination candidates nearby
vaccinate2 :: Universe2 DiseaseCell -> Universe2 DiseaseCell
vaccinate2 u2@(Universe2 u) = center'
  where center' = center2 u2

center2 :: Universe2 DiseaseCell -> Universe2 DiseaseCell
center2 (Universe2 u) = Universe2 . center . fmap center $ u

center :: Universe a -> Universe a
center u | offset u > center' = iterate left u !! (offset u - center')
         | offset u < center' = iterate right u !! (center' - offset u)
         | otherwise = u
  where center' = floor (fromIntegral (size u) / 2.0)

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

closest2 :: (a -> Bool) -> Universe2 a -> (Universe2 a, Universe2 a, Universe2 a, Universe2 a)
closest2 test (Universe2 u@(Universe s o ls x rs)) = (Universe2 u',
                                                      Universe2 d,
                                                      Universe2 (Universe s o ls l rs),
                                                      Universe2 (Universe s o ls r rs))
  where (l, r) = closest test (extract u)
        (u', d) = closest (test . extract) u

closest :: (a -> Bool) -> Universe a -> (Universe a, Universe a)
closest test u = (closestWith test left u, closestWith test right u)

closestWith :: (a -> Bool) -> (Universe a -> Universe a)
                           -> Universe a
                           -> Universe a
closestWith test f u | test (extract u) = u
                     | otherwise = closestWith test f (f u)
