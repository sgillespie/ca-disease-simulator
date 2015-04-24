module Disease.Vaccine where

import Control.Comonad
import Data.List

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
vaccinate2 u2@(Universe2 u) = Universe2 . vaccinate' fs $ start
  where (start, fs) = findOpening u
        vaccinate' fs@(f:fs') u = case extract (extract u) of
          Immune   -> vaccinate' fs (f u)
          Alive    -> getUniverse2 . replace2 Immune . Universe2 $ u
          Infected -> vaccinate' (changeDirection fs' (inverse2' f u)) (inverse2' f u)
          _        -> u

        changeDirection [] u = [id]
        changeDirection (f:fs) u | findWith test' f u == Infected = changeDirection fs u
                                 | otherwise = f:fs
        test' = (flip elem) [Infected, NotUsed, Alive] . extract . extract

findOpening :: Universe (Universe DiseaseCell) ->
               (Universe (Universe DiseaseCell), 
                [Universe (Universe DiseaseCell) -> Universe (Universe DiseaseCell)])
findOpening u | extract' topCenter /= Infected = (topCenter, topCenterFs)
              | extract' bottomCenter /= Infected = (bottomCenter, bottomCenterFs)
              | extract' leftCenter /= Infected = (leftCenter, leftCenterFs)
              | otherwise = (rightCenter, rightCenterFs)
  where topCenter = fmap center (moveToLeft u)
        topCenterFs = [right, fmap left, fmap right, left]
        bottomCenter = fmap center (moveToRight u)
        bottomCenterFs = [left, fmap right, fmap left,right]
        leftCenter = fmap moveToLeft (center u)
        leftCenterFs = [fmap right, left, right, fmap left]
        rightCenter = fmap moveToRight (center u)
        extract' u = (extract . extract) u
        rightCenterFs = [fmap left, right, left, fmap right]

inverse2 :: (Universe2 a -> Universe2 a) -> Universe2 a -> Universe2 a
inverse2 f u2@(Universe2 u) = (Universe2 . shiftedOuter . shiftedInner $ u)
  where shifted = getUniverse2 (f u2)
        outer = offset shifted - offset u
        inner = offset (extract shifted) - offset (extract u)
        shiftedOuter = moveTo (offset u - outer)
        shiftedInner = fmap (moveTo $ offset (extract u) - inner)

inverse2' :: (Universe (Universe a) -> Universe (Universe a))
             -> Universe (Universe a) -> Universe (Universe a)
inverse2' f u = getUniverse2 . inverse2 wrapF . Universe2 $ u
  where wrapF (Universe2 u') = Universe2 (f u')

findWith :: (Universe (Universe a) -> Bool)
            -> (Universe (Universe a) -> Universe (Universe a))
            -> Universe (Universe a)
            -> a
findWith test f = extract . extract . head . filter test . iterate f

closestAlive :: Universe DiseaseCell -> Maybe (Universe DiseaseCell)
closestAlive u | closest' == [] = Nothing -- Can't find one
               | otherwise = Just (minimumBy compare' closest')
  where (left', right') = closest (flip elem $ [Alive, NotUsed]) u
        closest' = filter (\u -> extract u /= NotUsed) [left', right']
        compare' a b = compare (distanceFrom u a) (distanceFrom u b)

closest :: (a -> Bool) -> Universe a -> (Universe a, Universe a)
closest test u = (l, r)
  where l = closestWith test left u
        r = closestWith test right u

