module Main where

import UCSemantic

main = do
  syms <- ucSemantic
  putStrLn $ concatMap (\x -> show x ++ "\n") syms
