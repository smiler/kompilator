module Main where

import UCRtl
import UCRtlAT
import UCParser
import UCSemantic

fromPrg (PROGRAM rtl) = rtl

main =
  getContents >>= (putStrLn . (concatMap show) . fromPrg . parse . ucSemantic . ucParser)  
