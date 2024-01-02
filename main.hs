module Main where

import Datastructs
import Interpreter
import Tests(runTests)

main :: IO ()
main = do
  runTests
  