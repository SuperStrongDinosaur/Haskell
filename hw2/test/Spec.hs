module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.TestUnit (hspecTestTree)
import Test.TestProperty

main :: IO ()
main = hspecTestTree >>= \unitTests ->
       let allTests = testGroup "Unit" $ [unitTests]  ++ okTestTree
       in defaultMain allTests