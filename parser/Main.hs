module Main where

import UCParser

main = do
  s <- ucParser
  putStrLn $ concatMap (\s -> show s ++ "\n") s
