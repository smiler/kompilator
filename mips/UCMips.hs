-- DO: fixa till stacken - hantera argument, pekare, temps osv
module UCMips where

import UCRtlAT as RTL
import Control.Monad

-- main function
funcode :: [RTL.Dec] -> [String]
funcode decs =
  "\t.data" :
  map loldata (filter (not.proc) decs) ++
    "\n\t.text" :
    "\t.globl main" :
    (join $ map lolcode (filter proc decs))

-- simple predicate function
proc :: RTL.Dec -> Bool
proc (RTL.PROC {}) = True
proc _ = False

-- handle data
loldata :: RTL.Dec -> String
loldata dec =
  label dec ++
  ": .space " ++ (show $ pad4 $ size dec)

pad4 :: Int -> Int
pad4 n = ((n - 1) `div` 4 + 1) * 4

-- handle procedures
lolcode :: RTL.Dec -> [String]
lolcode p =
  (label p ++ ":") :
  [ "\tadd\t$t0, $sp, $zero"
  , "\taddi\t$sp, $sp, -" ++ (show $ pad4 $ frameSize p) -- offset local arrays
  , "\tsw\t$fp, 0($sp)" -- save frame pointer
  , "\taddi\t$sp, $sp, -4" -- pop
  , "\tadd\t$fp, $t0, $zero" -- set frame pointer
  , "\tsw\t$ra, 0($sp)" -- save return address
  , "\taddi\t$sp, $sp, -4" -- pop
  , "\taddi\t$sp, $sp, -" ++ (show $ (*4) $ ((length $ locals p) + (length $ formals p))) -- offset local temporaries
  ] ++
  (fromRegArgs $ formals p) ++
  (fromStackArgs $ formals p) ++
  (join $ map lolins (insns p)) ++
  ["\taddi\t$sp, $sp, " ++ (show $ (*4) $ ((length $ locals p) + (length $ formals p)))
  ,"\tlw\t$ra, 4($sp)"
  ,"\tlw\t$fp, 8($sp)"
  ,"\taddi\t$sp, $sp, " ++ (show $ (+8) $ pad4 $ frameSize p)
  ,"\tjr\t$ra"
  ]

fromRegArgs :: [RTL.Temp] -> [String]
fromRegArgs f =
  fromRegArgs' (take 4 f) 0
  where
    fromRegArgs' [] _ = []
    fromRegArgs' (f:tl) n =
      ("\tsw\t$a" ++ show n ++ ", " ++ temp2stack f)
      : fromRegArgs' tl (succ n)

fromStackArgs :: [RTL.Temp] -> [String]
fromStackArgs f =
  fromStackArgs' (drop 4 f) 1
  where
    fromStackArgs' [] _ = []
    fromStackArgs' (f:tl) n =
      ("\tlw\t$t0, " ++ show (n * 4) ++ "($fp)") :
      ("\tsw\t$t0, " ++ temp2stack f) :
      fromStackArgs' tl (succ n)

-- find stack position for temporary
temp2stack :: RTL.Temp -> String
temp2stack (RTL.Temp 0) = error "lol"--"$v0"
temp2stack (RTL.Temp 1) = "$fp"
temp2stack (RTL.Temp t) = show (4 * (t - 1)) ++ "($sp)"


-- handle instructions
lolins :: RTL.Insn -> [String]
lolins (RTL.LABDEF l) = [l ++ ":"]
lolins (RTL.JUMP l) = ["\tj\t" ++ l]
lolins (RTL.CJUMP rop t1 t2 l) =
  ["\tlw\t$t0, " ++ temp2stack t1,
   "\tlw\t$t1, " ++ temp2stack t2,
   case rop of
     RTL.LT -> "\tblt\t$t0, $t1, " ++ l
     RTL.LE -> "\tble\t$t0, $t1, " ++ l
     RTL.EQ -> "\tbeq\t$t0, $t1, " ++ l
     RTL.NE -> "\tbne\t$t0, $t1, " ++ l
     RTL.GE -> "\tbge\t$t0, $t1, " ++ l
     RTL.GT -> "\tbgt\t$t0, $t1, " ++ l
  ]
lolins (RTL.STORE ty dst src) = 
  ["\tlw\t$t0, " ++ temp2stack dst,
   "\tlw\t$t1, " ++ temp2stack src,
   case ty of
     LONG -> "\tsw\t$t1, 0($t0)"
     BYTE -> "\tsb\t$t1, 0($t0)"
  ]
lolins (RTL.EVAL t e) =
  lolexpr e ++
  ["\tsw\t$v0, " ++ temp2stack t]
lolins (RTL.CALL dst l args) =
  regargs (take 4 args) ++
  pushargs (drop 4 args) ++
  ["\tjal\t" ++ l,
   "\taddi\t$sp, $sp, " ++ show (4 * cminus (length args) 4)] ++
   case dst of
     Nothing -> []
     Just t -> ["\tsw\t$v0, " ++ temp2stack t]

-- natural subtraction
cminus a b | a - b < 0 = 0
           | otherwise = a - b

-- put arguments in registers
regargs :: [RTL.Temp] -> [String]
regargs tl = 
  let
    n = length tl
  in
    reg' tl 0
  where
   reg' [] n = []
   reg' (hd : tl) n =
     ("\tlw\t$a" ++ show n ++ ", " ++ temp2stack hd)
     : reg' tl (n + 1)

-- push arguments to the stack
pushargs :: [RTL.Temp] -> [String]
pushargs tl =
  push' (reverse tl)
  where
   push' [] = []
   push' (hd : tl) =
     ("\tlw\t$t0, " ++ temp2stack hd)
     : ("\tsw\t$t0, 0($sp)")
     : ("\taddi\t$sp, $sp, -4")
     : push' tl

lolexpr :: RTL.Expr -> [String]
lolexpr (RTL.TEMP t) =
  ["\tlw\t$t0, " ++ temp2stack t,
   "\taddi\t$v0, $t0, 0"
  ]
lolexpr (RTL.ICON i) =
  ["\tli\t$v0, " ++ show i]
lolexpr (RTL.LABREF l) =
  ["\tla\t$v0, " ++ l]
lolexpr (RTL.UNARY (RTL.LOAD ty) t) =
  ["\tlw\t$t0, " ++ temp2stack t,
   case ty of
     LONG -> "\tlw\t$v0, 0($t0)"
     BYTE -> "\tlb\t$v0, 0($t0)"
  ]
lolexpr (RTL.BINARY binop t1 t2) =
  ("\tlw\t$t0, " ++ temp2stack t1) :
  case t2 of
    RTL.Temp 1 -> "\tadd\t$t1, $fp, $zero"
    otherwise -> "\tlw\t$t1, " ++ temp2stack t2
    :
  case binop of
            RTL.ADD -> ["\tadd\t$v0, $t0, $t1"]
            RTL.SUB -> ["\tsub\t$v0, $t0, $t1"]
            RTL.MUL -> ["\tmul\t$v0, $t0, $t1"]
            RTL.DIV -> ["\tdiv\t$v0, $t0, $t1"
                       ,"\tnop"
                       ,"\tnop"
                       ,"\tmflo\t$v0"
                       ]
