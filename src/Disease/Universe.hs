module Disease.Universe where

import Control.Comonad

-- |We represent a 1 dimensional automaton using a doubly infinite list
-- 1) The list to the left
-- 2) The focused element
-- 3) The list to the right
--
-- Example: [-1,-2,..] 0 [1,2,..] is the set of all integers focused at 0
data Universe a = Universe [a] a [a]
                deriving (Show)

fromList :: [a] -> Universe a
fromList (x:xs) = Universe [] x xs

instance Eq a => Eq (Universe a) where
  u1@(Universe ll lx lr) == u2@(Universe rl rx rr) = toList u1 == toList u2
    where toList (Universe ls x rs) = (reverse ls) ++ x:rs

-- |Shift the focus left one item
left :: Universe a -> Universe a
left (Universe (l:ls) x rs) = Universe ls l (x:rs)

-- |Shift the focus right one item
-- We assume infinite lists
right :: Universe a -> Universe a
right (Universe ls x (r:rs)) = Universe (x:ls) r rs

neighbors :: Universe a -> [a]
neighbors (Universe [] x [])         = [x]
neighbors (Universe (l:ls) x [])     = [l, x]
neighbors (Universe [] x (r:rs))     = [x, r]
neighbors (Universe (l:ls) x (r:rs)) = [l, x, r]

instance Functor Universe where
  fmap f (Universe ls x rs) = Universe (fmap f ls) (f x) (fmap f rs)

instance Comonad Universe where
  extract (Universe _ x _) = x

  duplicate u = Universe (lduplicate u) u (rduplicate u)
    where lduplicate (Universe [] _ _) = []
          lduplicate u = (left u):(lduplicate . left $ u)
  
          rduplicate (Universe _ _ []) = []
          rduplicate u = (right u):(rduplicate . right $ u)
  
-- |We represent a 2 dimensional automaton as a Universe of Universes
newtype Universe2 a = Universe2 (Universe (Universe a))
                    deriving (Show)

getUniverse2 :: Universe2 a -> (Universe (Universe a))
getUniverse2 (Universe2 u) = u

fromList2 :: [[a]] -> Universe2 a
fromList2 (l:ls) = Universe2 $ Universe [] (fromList l) (fmap fromList ls)

instance Eq a => Eq (Universe2 a) where
  a == b = getUniverse2 a == getUniverse2 b

instance Functor Universe2 where
  fmap f (Universe2 u) = Universe2 . (fmap . fmap) f $ u

instance Comonad Universe2 where
  extract (Universe2 u) = (extract . extract) u

  -- Assumption: All sub-universes are the same length
  duplicate (Universe2 u) = fmap Universe2 . Universe2 . shifted . shifted $ u
    where shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
          shifted u = Universe (take (llen u) $ tail $ iterate (fmap left) u)
                      u
                      (take (rlen u) $ tail $ iterate (fmap right) u)

          llen (Universe ((Universe ls _ _):_) _ _) = length ls
          llen _ = 0
          
          rlen (Universe _ _ ((Universe _ _ rs):_)) = length rs
          rlen _ = 0

neighbors2 :: Universe2 a -> [a]
neighbors2 (Universe2 u@(Universe [] x []))     = neighbors x
neighbors2 (Universe2 u@(Universe [] x (r:rs))) = neighbors x ++ [extract r]
neighbors2 (Universe2 u@(Universe (l:ls) x [])) = [extract l] ++ neighbors x
neighbors2 (Universe2 u@(Universe (l:ls) x (r:rs))) =
  [extract l] ++ neighbors x ++ [extract r]

