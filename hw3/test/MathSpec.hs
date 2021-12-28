module MathSpec where

import Math

import qualified Data.Map.Strict as M
import Test.Hspec

spec :: Spec
spec = do
  let m1 = M.singleton "x" 1
  let m0 = M.empty
  it "lit" $ do
    doEval (Lit 1) m1 `shouldReturn` 1
  it "var" $ do
    doEval (Var "x") m1 `shouldReturn` 1
    doEval (Var "x") m0 `shouldThrow` (== NoVarBindingFound "x")
    doEval (Var "y") m1 `shouldThrow` (== NoVarBindingFound "y")
  it "add" $ do
    doEval (Var "x" `Add` Lit 5) m1 `shouldReturn` 6
    doEval (Var "x" `Add` Lit 2) m0 `shouldThrow` (== NoVarBindingFound "x")
  it "div" $ do
    doEval (Var "x" `Div` Lit 0) m1 `shouldThrow` (== DivizionByZero)
    doEval (Lit 5 `Div` Lit 2) m0 `shouldReturn` 2
  it "add" $ do
    doEval (Var "x" `Add` Lit 0) m1 `shouldReturn` 1
    doEval (Lit 2 `Add` Lit 2) m0 `shouldReturn` 4
  it "let" $ do
    doEval ("x" `Let` Lit 2 $ Var "x") m0 `shouldReturn` 2
    doEval ("y" `Let` Lit 2 $ Var "x" `Add` Var "y") m1 `shouldReturn` 3
    doEval (Var "x" `Add` (Lit 3 `Mul` ("x" `Let` Lit 2 $ Var "x"))) m1 `shouldReturn` 7
    doEval (Let "y" (Lit 2 `Add` Lit 2) (Var "y" `Add` (Lit 3 `Mul` ("x" `Let` Lit 2 $ Var "x")))) m0 `shouldReturn` 10