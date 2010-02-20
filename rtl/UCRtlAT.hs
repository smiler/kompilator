module UCRtlAT (Program(..), Dec(..), Insn(..), Expr(..), Binop(..),
       Unop(..), Relop(..), Temp(..), Label, Ty(..), rv, fp, sizeof) where

import Prelude hiding (LT, EQ, GT)

data Ty	 -- for memory load/store operations
  = BYTE -- char 
  | LONG -- int or pointer 
instance Show Ty where
  show BYTE = "_b"
  show LONG = "_l"

newtype Temp = Temp Int	    -- pseudo register
  deriving (Eq, Ord)
instance Show Temp where
  show (Temp t) = "#" ++ show t
instance Enum Temp where
  succ (Temp t) = Temp (succ t)

type Label = String -- label

rv :: Temp -- temp for return value 
rv = Temp 0
fp :: Temp -- frame pointer temp, for storage of local arrays 
fp = Temp 1

data Relop 
  = LT | LE | EQ | NE | GE | GT
instance Show Relop where
  show LT = "lt"
  show LE = "le"
  show EQ = "eq"
  show NE = "ne"
  show GE = "ge"
  show GT = "gt"

data Unop
  = LOAD Ty	-- load value of type 'ty' from address in operand 
instance Show Unop where
  show (LOAD ty) = "load" ++ show ty

data Binop
  = ADD | SUB | MUL | DIV
instance Show Binop where
  show ADD = "add"
  show SUB = "sub"
  show MUL = "mul"
  show DIV = "div"

data Expr
  -- moving values into the result temp 
  = TEMP Temp
  | ICON Int
  | LABREF Label
    -- unary operators 
  | UNARY Unop Temp
    -- binary operators 
  | BINARY Binop Temp Temp
instance Show Expr where
  show (TEMP t) = show t
  show (ICON i) = show i
  show (LABREF l) = show l
  show (UNARY op src) = show op ++ " " ++ show src
  show (BINARY bop src1 src2) = show bop ++ " " ++ show src1 ++ " " ++ show src2

type MaybeTemp = Maybe Temp

data Insn
  -- control flow 
  = LABDEF Label
  | JUMP Label
  | CJUMP Relop Temp Temp Label
    -- stores to memory 
  | STORE Ty Temp Temp
    -- simple expression evaluation 
  | EVAL Temp Expr
    -- function calls: could be expr but result temp is optional 
  | CALL MaybeTemp Label [Temp]
instance Show Insn where
  show (LABDEF l) = show l ++ ":\n"
  show (JUMP l) = "\t(goto " ++ show l ++ ")\n"
  show (CJUMP op src1 src2 l) = "\t(if (" ++ show op ++ " " ++ 
                              show src1 ++ " " ++ show src2 ++
                              ") (goto " ++ show l ++ "))\n"
  show (STORE ty dst src) = "\t(store " ++ show ty ++ " " ++ 
                          show dst ++ " " ++ show src ++ ")\n"
  show (EVAL dst expr) = "\t(set " ++ show dst ++ " " ++ show expr ++ ")\n"
  show (CALL (Just dst) l args) = "\t(set " ++ show dst ++ " (call " ++ show l ++
                                " " ++ unwords (map show args) ++ ")\n"
  show (CALL Nothing l args) = "\t(call " ++ show l ++ 
                             " " ++ unwords (map show args) ++ ")\n"
    
data Dec
  = PROC {label :: Label, 
          formals :: [Temp], 
          locals :: [Temp],
          frameSize :: Int, 
          insns :: [Insn]}
  | DATA {label :: Label, 
          size :: Int}
instance Show Dec where
  show p@(PROC{}) = "\n(procedure " ++ show (label p) ++
              "\n\t(formals " ++ (unwords $ map show (formals p)) ++
              ")\n\t(locals " ++ (unwords $ map show (locals p)) ++
              ")\n\t(frameSize " ++ show (frameSize p) ++ ")\n" ++
              (unwords $ map show (insns p)) ++ ")\n"
  show d@(DATA{}) = "\n(data " ++ show (label d) ++ " " ++ show (size d) ++ ")\n"

data Program
  = PROGRAM [Dec]
instance Show Program where
  show (PROGRAM ds) = unwords $ map show ds
--newLabel :: a -> Label
--newTemp :: a -> Temp

sizeof :: Ty -> Int
sizeof BYTE = 1
sizeof LONG = 4
