module Disease.Universe where

import Control.Comonad

-- |We represent a 1 dimensional automaton using a doubly infinite list
-- 1) The size of the universe
-- 1) Current offset
-- 1) The list to the left
-- 2) The focused element
-- 3) The list to the right
--
-- Example: [-1,-2,..] 0 [1,2,..] is the set of all integers focused at 0
--
-- Assumption: Always use infinite lists
data Universe a = Universe Int Int [a] a [a]
                deriving (Show)

size :: Universe a -> Int
size (Universe s _ _ _ _ ) = s

offset :: Universe a -> Int
offset (Universe _ o _ _ _) = o

-- |We represent a 2 dimensional automaton as a Universe of Universes
newtype Universe2 a = Universe2 (Universe (Universe a))
                    deriving (Show)

getUniverse2 :: Universe2 a -> (Universe (Universe a))
getUniverse2 (Universe2 u) = u

instance Eq a => Eq (Universe a) where
  u1 == u2 = toList u1 == toList u2

instance Functor Universe where
  fmap f (Universe size offset ls x rs) = Universe size offset (fmap f ls) (f x) (fmap f rs)

instance Comonad Universe where
  extract (Universe _ _ _ x _) = x
  duplicate u = Universe (size u) (offset u) (tail . iterate left $ u) u (tail . iterate right $ u)

instance Eq a => Eq (Universe2 a) where
  a == b = getUniverse2 a == getUniverse2 b

instance Functor Universe2 where
  fmap f (Universe2 u) = Universe2 . (fmap . fmap) f $ u

instance Comonad Universe2 where
  extract (Universe2 u) = (extract . extract) u
  duplicate (Universe2 u) = undefined

fromList :: [a] -> a -> Universe a
fromList l@(x:xs) n = Universe (length l) 0 (repeat n) x (xs ++ repeat n)

fromList2 :: [[a]] -> a -> Universe2 a
fromList2 ls n = Universe2 . (flip fromList $ n') . fmap (flip fromList $ n) $ ls
  where n' = Universe 0 0 (repeat n) n (repeat n)

toList :: Universe a -> [a]
toList (Universe size offset ls x rs) = ls' ++ x' ++ rs'
  where ls' = reverse . drop (offset-size) . take offset $ ls
        rs' = drop (-offset-1) (take (size-offset-1) rs)
        x'  | offset < 0 || offset >= size = []
            | otherwise = [x]

-- |Shift the focus left one item
left :: Universe a -> Universe a
left (Universe size offset (l:ls) x rs) = Universe size (offset-1) ls l (x:rs)

-- |Shift the focus right one item
-- We assume infinite lists
right :: Universe a -> Universe a
right (Universe size offset ls x (r:rs)) = Universe size (offset+1) (x:ls) r rs

neighbors :: Universe a -> [a]
neighbors u = map (extract . apply u) [left, id, right] 
  where apply x f = f x

neighbors2 :: Universe2 a -> [a]
neighbors2 (Universe2 u@(Universe size offset (l:ls) x (r:rs)))
  | offset == 0 && size == 1 = neighbors x
  | offset == 0              = neighbors x ++ [extract r]
  | offset >= (size-1)       = [extract l] ++ neighbors x
  | otherwise                = [extract l] ++ neighbors x ++ [extract l]