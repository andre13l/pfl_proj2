module Main where

import Datastructs
import Interpreter
import Tests( runTests, runTests2)

main :: IO ()
main = do
  runTests
  runTests2