module Main where

import UCSemantic
import UCParser

main = do
--  getContents >>= ucParser >>= ucSemantic >>= (putStrLn . concatMap (\x -> show x ++ "\n"))
  getContents >>= (putStrLn . concatMap (\x -> show x ++ "\n") . ucSemantic . ucParser)
  putStrLn "OK."
