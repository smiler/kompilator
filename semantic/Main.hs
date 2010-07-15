module Main where

import UCSemantic
import UCParser

main = do
  parseTree <- ucParser
  syms <- ucSemantic parseTree
  putStrLn $ concatMap (\x -> show x ++ "\n") syms
