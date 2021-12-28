module Test.TestProperty
       ( okTestTree
       ) where

import Hedgehog (Gen, MonadTest, Property, assert, forAll, property)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Task1

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

okTestTree :: [TestTree]
okTestTree = [ testProperty "bin test" prop_Bin
             ]

checkDiff2 :: [Int] -> [[Int]] -> Bool
checkDiff2 a (x : xs) = (a /= x && checkDiff2 a xs)
checkDiff2 _ [] = True

checkDiff1 :: [[Int]] -> Bool
checkDiff1 (x : xs) = (checkDiff2 x xs && checkDiff1 xs)
checkDiff1 [] = True

checkBin :: (MonadTest m) => Int -> [[Int]] -> m ()
checkBin n x = assert $ checkDiff1 x && (length x == 2 ^ n)

genInt :: Gen Int
genInt = Gen.int (Range.constant 0 14)

prop_Bin :: Property
prop_Bin = property $
  forAll genInt >>= \a ->
  checkBin a $ Task1.bin a


  
