module Main where

import qualified Data.ByteString as BS 
import Parser              
import System.Environment 

main :: IO ()
main = do
  args <- getArgs
  let name =  head args
  program <- BS.readFile name
  runProgram program

-- cabal new-run interpreter "test.txt"