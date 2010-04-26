module UCMips where

import UCRtlAT as RTL
import Control.Monad

-- simple predicate function
proc :: RTL.Dec -> Bool
proc p@(RTL.PROC {}) = True
proc _ = False

-- handle data
loldata :: RTL.Dec -> String
loldata dec =
  label dec ++
    ": .byte " ++
--    join (take (ceiling $ (fromIntegral $ size dec)/4 - 1) 
    join (take (size dec - 1) 
               (map ((++",").show) [0,0..])) ++
    "0"

-- handle procedures
lolcode :: RTL.Dec -> String
lolcode _ = ""

funcode :: [RTL.Dec] -> [String]
funcode decs =
  "\t.data" :
  map loldata (filter (not.proc) decs) ++
    "\n\t.text" :
    "\t.globl main" :
    map lolcode (filter proc decs)
