module Main where

import UCRtl
import UCRtlAT
import UCParser

main = do
  tree <- ucParser
  let PROGRAM rtl = parse tree
  putStrLn $ concatMap show rtl
