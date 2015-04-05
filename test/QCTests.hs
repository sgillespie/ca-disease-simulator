module QCTests (tests) where
-- TODO: Refactor and enable universe tests
-- TODO: Fix and enable universe2 tests


import Control.Comonad

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

import Disease.Universe

instance (Arbitrary a) => Arbitrary (Universe a) where
  arbitrary = do
    x      <- arbitrary
    size   <- arbitrary
    offset <- arbitrary
    ls     <- infiniteListOf arbitrary
    rs     <- infiniteListOf arbitrary
    return $ Universe size offset ls x rs

instance (Arbitrary a) => Arbitrary (Universe2 a) where
  arbitrary = do
    x <- arbitrary
    return $ Universe2 x

tests :: IO [Test]
tests = return [testGroup "Universe tests" universeTests,
                testGroup "Universe2 tests" universe2Tests]
  where universeTests = [testProperty "extract = focused element"  prop_extract_id,
                         testProperty "extract . left = head ls"   prop_left_id,
                         testProperty "left u = u"                 prop_left_eq,
                         testProperty "extract . right = right ls" prop_right_id,
                         testProperty "right u = u"                prop_right_eq,
                         testProperty "fmap id u = u"              prop_fmap_id,
                         testProperty "extract . duplicate = u"    prop_duplicate_id,
                         testProperty "size = size . duplicate"    prop_duplicate_len,
                         testProperty "neighbors u = [left, extract, right]"
                                                                   prop_neighbors_id]
        universe2Tests = [testProperty "extract . duplicate = u"   prop_duplicate2_id,
                          testProperty "size = size . duplicate"   prop_duplicate2_len,
                          testProperty "neighbors u = [up, left, extract, right, down]"
                                                                   prop_neighbors2_id]

-- -- --
-- Universe Tests
prop_extract_id :: Universe Integer -> Bool
prop_extract_id u@(Universe _ _ _ x _) = extract u == x

prop_left_id :: Universe Integer -> Property
prop_left_id u@(Universe _ _ ls _ _)
  = (not . null) ls ==> (extract . left) u == (head ls)

prop_left_eq :: Universe Integer -> Property
prop_left_eq u@(Universe _ _ ls x rs) = (not . null) ls ==> u == left u

prop_right_id :: Universe Integer -> Property
prop_right_id u@(Universe _ _ _ _ rs)
  = (not . null) rs ==> (extract . right) u == (head rs)

prop_right_eq :: Universe Integer -> Property
prop_right_eq u@(Universe _ _ ls x rs) = (not . null) rs ==> u == right u

prop_fmap_id :: Universe Integer -> Bool
prop_fmap_id u = fmap id u == u

prop_duplicate_id :: Universe Integer -> Bool
prop_duplicate_id u = u == (extract . duplicate) u

prop_duplicate_len :: Universe Integer -> Bool
prop_duplicate_len u = size u == (size . duplicate) u

prop_neighbors_id :: Universe Integer -> Bool
prop_neighbors_id u = neighbors u == map extract [left u, u, right u]

prop_duplicate2_id :: Universe2 Integer -> Bool
prop_duplicate2_id u = size (getUniverse2 u) == (size . getUniverse2 . duplicate) u

prop_duplicate2_len :: Universe2 Integer -> Bool
prop_duplicate2_len u = (size . getUniverse2) u == (size . getUniverse2 . duplicate) u

prop_neighbors2_id :: Universe2 Integer -> Bool
prop_neighbors2_id u = neighbors2 u == [extract . extract . left . getUniverse2 $ u] ++
                                       (neighbors . extract . getUniverse2) u ++
                                       [extract . extract . right . getUniverse2 $ u]

-- -- --
-- CA Tests
