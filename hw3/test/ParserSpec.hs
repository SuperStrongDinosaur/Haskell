{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Math
import Parser
import Program
import qualified Data.Map.Strict as M
import Test.Hspec

spec :: Spec
spec = do
  it "2 + 2" $ 
    parseAndEval "2 + 2" `shouldReturn` 4
  it "x + 2" $ 
    parseAndEval "x + 2" `shouldThrow` (== NoVarBindingFound "x")
  it "1+  (     let x  =2 in x   )  " $ 
    parseAndEval "10+  (     let x  =2 in x   )  " `shouldReturn` 12
  it "mut x = 2 + 2 * 2 / 2 - 1" $ 
    parseAndCompute "mut x = 2 + 2 * 2 / 2 - 1" `shouldReturn` M.singleton "x" 3
  it "10 + 3 * (let x = 2 in x)" $ 
    parseAndEval "10 + 3 * (let x = 2 in x)" `shouldReturn` 16
  it "x = 0" $ 
    parseAndCompute "x = 0" `shouldThrow` (== UndefinedVarError "x")
