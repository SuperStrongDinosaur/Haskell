module Test.TestTask1
       ( hspecTestTree
       ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

import Prelude

import Task1
import Task2
import Task3

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "Unit" spec_unit

c0 :: Expr
c0 = Task1.const 0
c1 :: Expr
c1 = Task1.const 1
c2 :: Expr
c2 = Task1.const 2
c4 :: Expr
c4 = Task1.const 4
c_1 :: Expr
c_1 = Task1.const (-1)

spec_unit :: Spec
spec_unit = do
  describe "Arithmetic" $ do
    it "2" $
      eval c2 `shouldBe` Right 2
    it "0" $
      eval c0 `shouldBe` Right 0
    it "-1" $
      eval c_1 `shouldBe` Right (-1)
    it "1 + 2" $
      eval (c1 `Task1.add` c2) `shouldBe` Right 3
    it "1 - 2" $
      eval (c1 `Task1.sub` c2) `shouldBe` Right (-1)
    it "4 / 2" $
      eval (c4 `Task1.div` c2) `shouldBe` Right 2
    it "4 / 0" $
      eval (c4 `Task1.div` c0) `shouldBe` Left DivByZero
    it "0 / 2" $
      eval (c0 `Task1.div` c4) `shouldBe` Right 0
    it "2 ^ 4" $
      eval (c2 `Task1.pow` c4) `shouldBe` Right 16
    it "2 ^ 0" $
      eval (c2 `Task1.pow` c0) `shouldBe` Right 1
    it "2 ^ -1" $
      eval (c2 `Task1.pow` c_1) `shouldBe` Left PowByNeg

  describe "stringSum" $ do
    it "5 5 +5 -5" $
      stringSum "5 5 +5 -5" `shouldBe` Just 10
    it "5\n\t 5 \t\n+5 \n\t\t\n-5" $
      stringSum "5\n\t 5 \t\n+5 \n\t\t\n-5" `shouldBe` Just 10
    it "5 5 +5 lul -5" $
      stringSum "5 5 +5 lul -5" `shouldBe` Nothing

  describe "Parsers" $ do
    it "ok \"lul\"" $
      runParser ok "lul" `shouldBe` Just ((), "lul")
    it "eof \"lul\"" $
      runParser eof "lul" `shouldBe` Nothing
    it "eof \"\"" $
      runParser eof "" `shouldBe` Just ((), "")
    it "element \'a\' \"lul\"" $
      runParser (element 'a') "lul" `shouldBe` Nothing
    it "element \'l\' \"lul\"" $
      runParser (element 'l') "lul" `shouldBe` Just ('l', "ul")
    it "stream \"lu\" \"lul\"" $
      runParser (stream "lu") "lul" `shouldBe` Just ("lu", "l")
    it "stream \"la\" \"lul\"" $
      runParser (stream "la") "lul" `shouldBe` Nothing
    it "nat \"+42\" " $
      runParser nat "+42" `shouldBe` Just (42, "")
    it "nat \"-42\" " $
      runParser nat "-42" `shouldBe` Just (-42, "")
    it "brackets \"(()())()\"" $
      runParser brackets "(()())()" `shouldBe` Just ("(()())()", "")
    it "brackets \")(\"" $
      runParser brackets ")(" `shouldBe` Nothing





 