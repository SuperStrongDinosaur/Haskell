module LensSpec where

import Lens
import Data.Function 
import Test.Hspec

spec :: Spec
spec = do
  it "view" $ do
    (4, 2)^._1 `shouldBe` 4
    (4, 2)^._2 `shouldBe` 2
  it "set" $ do
    ((4, 2) & _2 .~ 42) `shouldBe` (4, 42)
    ((4, 2) & _1 .~ 42) `shouldBe` (42, 2)
  it "over" $ do
    ((4,2) & _1 %~ (+ 1)) `shouldBe` (5,2)
    ((4,2) & _2 %~ (+ 1)) `shouldBe` (4,3)