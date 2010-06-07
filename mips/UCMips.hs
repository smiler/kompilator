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
  ": .space " ++ (show $ size dec)

-- handle procedures
lolcode :: RTL.Dec -> [String]
lolcode p =
  (label p ++ ":") :
  "\tsw\t$ra, -4($sp)" :
  ("\taddi\t$sp, $sp, -" ++ show (frameSize p + (3 + (length $ locals p)) * 4)) :
  ("\taddi\t$t0, $sp, " ++ show ((3 + (length $ locals p)) * 4)) :
  "\tsw\t$t0, 4($sp)" :
  (join $ map lolins (insns p)) ++
  ["\taddi\t$sp, $sp, " ++ show (frameSize p + (3 + (length $ locals p)) * 4),
   "\tlw\t$ra, -4($sp)",
   "\tjr\t$ra"
  ]

cminus a b | a - b < 0 = 0
           | otherwise = a - b

lolins :: RTL.Insn -> [String]
lolins (RTL.LABDEF l) = [l ++ ":"]
lolins (RTL.JUMP l) = ["\tj\t" ++ l]
lolins (RTL.CJUMP rop (RTL.Temp t1) (RTL.Temp t2) l) =
  ["\tlw\t$t0, " ++ show (t1 * 4) ++ "($sp)",
   "\tlw\t$t1, " ++ show (t2 * 4) ++ "($sp)",
   case rop of
     RTL.LT -> "\tblt\t$t0, $t1, " ++ l
     RTL.LE -> "\tble\t$t0, $t1, " ++ l
     RTL.EQ -> "\tbeq\t$t0, $t1, " ++ l
     RTL.NE -> "\tbne\t$t0, $t1, " ++ l
     RTL.GE -> "\tbge\t$t0, $t1, " ++ l
     RTL.GT -> "\tbgt\t$t0, $t1, " ++ l
  ]
lolins (RTL.STORE ty (RTL.Temp dst) (RTL.Temp src)) = 
  ["\tlw\t$t0, " ++ show (dst * 4) ++ "($sp)",
   "\tlw\t$t1, " ++ show (src * 4) ++ "($sp)",
   case ty of
     LONG -> "\tsw\t$t1, 0($t0)"
     BYTE -> "\tsb\t$t1, 0($t0)"
  ]
{-
  ["\tlw\t$t0, " ++ show (dst * 4) ++ "($sp)",
   "\tlw\t$t1, " ++ show (src * 4) ++ "($sp)",
   "\tmul\t$t0, $t0, 4",
   "\tadd\t$t0, $t0, $sp",
   "\tsw\t$t1, 0($t0)"
  ]
-}
lolins (RTL.EVAL (RTL.Temp t) e) =
  lolexpr e ++
  ["\tsw\t$v0, " ++ show (t * 4) ++ "($sp)"]
lolins (RTL.CALL dst l args) = -- TODO ololol
  regargs (take 4 args) ++
  pushargs (drop 4 args) ++
  ["\taddi\t$sp, $sp, -" ++ show (cminus (length args) 5),
   "\tjal\t" ++ l,
   "\taddi\t$sp, $sp, " ++ show (cminus (length args) 5)] ++
   case dst of
     Nothing -> []
     Just (RTL.Temp t) -> ["\tsw\t$v0, " ++ show (t * 4) ++ "($sp)"]

regargs :: [RTL.Temp] -> [String]
regargs tl = 
  let
    n = length tl
  in
    reg' tl 0
  where
   reg' [] ack = []
   reg' ((Temp hd):tl) ack =
     ("\tlw\t$a" ++ show ack ++ ", " ++ show (hd * 4) ++ "($sp)") :
     reg' tl (ack + 1)

pushargs :: [RTL.Temp] -> [String]
pushargs tl =
  let
    n = length tl
  in
    push' tl 0
  where
   push' [] ack = []
   push' ((Temp hd):tl) ack =
     ("\tlw\t$t" ++ show ack ++ ", " ++ show (hd * 4) ++ "($sp)") :
     ("\tsw\t$t" ++ show ack ++ ", " ++ show (ack * 4) ++ "($sp)") :
     push' tl (ack + 1)
    

lolexpr :: RTL.Expr -> [String]
lolexpr (RTL.TEMP (RTL.Temp t)) =
  ["\tlw\t$t0, " ++ show (t * 4) ++ "($sp)",
   "\taddi\t$v0, $t0, 0"
  ]
lolexpr (RTL.ICON i) =
  ["\tli\t$v0, " ++ show i]
lolexpr (RTL.LABREF l) =
  ["\tla\t$v0, " ++ l]
lolexpr (RTL.UNARY (RTL.LOAD ty) (RTL.Temp t)) =
  ["\tlw\t$t0, " ++ show (t * 4) ++ "($sp)",
   case ty of
     LONG -> "\tlw\t$v0, 0($t0)"
     BYTE -> "\tlb\t$v0, 0($t0)"
  ]
lolexpr (RTL.BINARY binop (RTL.Temp t1) (RTL.Temp t2)) =
  ("\tlw\t$t0, " ++ show (t1 * 4) ++ "($sp)") :
  ("\tlw\t$t1, " ++ show (t2 * 4) ++ "($sp)") :
  case binop of
            RTL.ADD -> ["\tadd\t$v0, $t0, $t1"]
            RTL.SUB -> ["\tsub\t$v0, $t0, $t1"]
            RTL.MUL -> ["\tmul\t$v0, $t0, $t1"]
            RTL.DIV -> ["\tdiv\t$v0, $t0, $t1",
                        "\tnop",
                        "\tnop",
                        "\tmflo\t$v0"
                       ]
