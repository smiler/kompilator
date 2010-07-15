module Main where

import UCParser

main = do
  getContents >>= (\s -> putStrLn $ concatMap (\s -> show s ++ "\n") (ucParser s))
