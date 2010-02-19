module Main where

import UCParser
import UCRtlAT
import UCRtl

main = do
  tree <- ucParser
  let PROGRAM rtl = parse tree
  putStrLn $ concatMap show rtl
--  syms <- ucSemantic
--  putStrLn $ concatMap (\x -> show x ++ "\n") syms
--  putStrLn "olololol"
