module Main where

import UCRtl
import UCRtlAT
import UCParser
import UCSemantic
import UCMips

main =
  getContents >>= ((mapM_ putStrLn) . funcode . fromPrg . parse . ucSemantic . ucParser)
  where
   fromPrg (PROGRAM rtl) = rtl
