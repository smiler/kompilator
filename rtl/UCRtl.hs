module UCRtl where


data Ty	 -- for memory load/store operations
  = BYTE -- char 
  | LONG -- int or pointer 

type Temp = Int	    -- pseudo register 
type Label = String -- label

RV :: Temp -- temp for return value 
RV = 0
FP :: Temp -- frame pointer temp, for storage of local arrays 
FP = 1

data Relop 
  = LT | LE | EQ | NE | GE | GT

data Unop
  = LOAD Ty	-- load value of type 'ty' from address in operand 

data Binop
  = ADD | SUB | MUL | DIV

data Expr
  -- moving values into the result temp 
  = TEMP Temp
  | ICON Int
  | LABREF Label
    -- unary operators 
  | UNARY Unop Temp
    -- binary operators 
  | BINARY Binop Temp Temp
    
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
  | CALL Temp Option Label Temp List
    
data Dec
  = PROC {label :: Label, 
          formals :: [Temp], 
          locals :: [Temp],
          frameSize :: Int, 
          insns :: [Insn]}
  | DATA {label :: Label, 
          size :: Int}

data Program
  = PROGRAM [Dec]

--newLabel :: a -> Label
--newTemp :: a -> Temp

sizeof :: Ty -> Int
sizeof BYTE = 1
sizeof LONG = 4
