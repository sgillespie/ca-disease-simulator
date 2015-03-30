module QCTests (tests) where

import Control.Comonad

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

import Disease.Universe

instance (Arbitrary a) => Arbitrary (Universe a) where
  arbitrary = do
    x  <- arbitrary
    ls <- vectorOf 5 arbitrary
    rs <- listOf arbitrary
    return $ Universe ls x rs

instance (Arbitrary a) => Arbitrary (Universe2 a) where
  arbitrary = do
    x <- arbitrary
    return $ Universe2 x

tests :: IO [Test]
tests = return [testGroup "Universe tests" universeTests,
                testGroup "Universe2 tests" universe2Tests]
  where universeTests = [testProperty "extract id" prop_extract_id,
                         testProperty "left id" prop_left_id,
                         testProperty "left u == u" prop_left_eq,
                         testProperty "right id" prop_right_id,
                         testProperty "right u == u" prop_right_eq,
                         testProperty "fmap id" prop_fmap_id,
                         testProperty "duplicate id" prop_duplicate_eq,
                         testProperty "length . right . duplicate == length . right"  prop_duplicate_len]
        universe2Tests = []
          
-- -- --
-- Universe Tests
prop_extract_id :: Universe Integer -> Bool
prop_extract_id u@(Universe _ x _) = extract u == x

prop_left_id :: Universe Integer -> Property
prop_left_id u@(Universe ls _ _)
  = (not . null) ls ==> (extract . left) u == (head ls)

prop_left_eq :: Universe Integer -> Property
prop_left_eq u@(Universe ls x rs) = (not . null) ls ==> u == left u

prop_right_id :: Universe Integer -> Property
prop_right_id u@(Universe _ _ rs)
  = (not . null) rs ==> (extract . right) u == (head rs)

prop_right_eq :: Universe Integer -> Property
prop_right_eq u@(Universe ls x rs) = (not . null) rs ==> u == right u

prop_fmap_id :: Universe Integer -> Bool
prop_fmap_id u = fmap id u == u

prop_duplicate_eq :: Universe Integer -> Bool
prop_duplicate_eq u = case (duplicate u) of
  Universe ls x rs -> all (== x) (ls ++ rs)

prop_duplicate_len :: Universe Integer -> Bool
prop_duplicate_len u@(Universe ls x rs) = case (duplicate u) of
  Universe ls' x' rs' -> length ls' == length ls && length rs' == length rs

prop_duplicate2_eq :: Universe2 Integer -> Bool
prop_duplicate2_eq u = case (duplicate u) of
  Universe2 (Universe ls x rs) -> all (==x) (ls ++ rs)

prop_duplicate2_len :: Universe2 Integer -> Bool
prop_duplicate2_len u@(Universe2 (Universe ls x rs)) = case (getUniverse2 . duplicate $ u) of
  Universe ls' x' rs' -> length ls' == length ls && length rs' == length rs

-- -- --
-- CA Tests
