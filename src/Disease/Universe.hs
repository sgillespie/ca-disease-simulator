module Disease.Universe where

import qualified Control.Comonad as C

-- |We represent a 1 dimensional automaton using a doubly infinite list
-- 1) The infinite list to the left
-- 2) The focused element
-- 3) The infinite list to the right
--
-- Example: [-1,-2,..] 0 [1,2,..] is the set of all integers focused at 0
data Universe a = Universe [a] a [a]
                deriving (Show)

-- |Shift the focus left one item
-- We assume infinite lists
left :: Universe a -> Universe a
left (Universe (l:ls) x rs) = Universe ls l (x:rs)

-- |Shift the focus right one item
-- We assume infinite lists
right :: Universe a -> Universe a
right (Universe ls x (r:rs)) = Universe (x:ls) r rs

-- |Extract the focused element
extract :: Universe a -> a
extract (Universe _ x _) = x

instance Functor Universe where
  fmap f (Universe ls x rs) = Universe (fmap f ls) (f x) (fmap f rs)

-- |Universe of all shifts
duplicate :: Universe a -> Universe (Universe a)
duplicate u = Universe (tail $ iterate left u) u (tail $ iterate right u)

-- |We represent a 2 dimensional automaton as a Universe of Universes
newtype Universe2 a = Universe2 (Universe (Universe a))
                    deriving (Show)

instance Functor Universe2 where
  fmap f (Universe2 u) = Universe2 . (fmap . fmap) f $ u

instance C.Comonad Universe2 where
  extract (Universe2 u) = (extract . extract) u
  
  duplicate (Universe2 u) = fmap Universe2 . Universe2 . shifted . shifted $ u
    where shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
          shifted u = Universe (tail . iterate (fmap left) $ u)
                               u
                               (tail . iterate (fmap right) $ u)
