module Main where

import UCRtl
import UCRtlAT
import UCParser
import UCMips

main = do
  tree <- ucParser
  let PROGRAM rtl = parse tree
  mapM_ putStrLn $ funcode rtl
