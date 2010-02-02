module UCParserAST (Program, Topdec(..), Stmt(..), Expr(..), Binop(..),
                    Unop(..), Vardec(..), Type(..)) where

data Type
  = INT
  | CHAR
  | VOID
  deriving Show
{-
instance Show Type where
  show INT  = "int"
  show CHAR = "char"
  show VOID = "void"
-}
type MaybeInt = Maybe Int
data Vardec
  = SCALARDEC Type String
  | ARRAYDEC  Type String MaybeInt
instance Show Vardec where
  show (SCALARDEC t s) = show t ++ " " ++ show s
  show (ARRAYDEC t s Nothing) = show t ++ "[]" ++ " " ++ show s
  show (ARRAYDEC t s (Just i)) = show t ++ "[" ++ show i ++ "]" ++ " " ++ show s

data Binop
  = ADD | SUB | MUL | DIV
  | LT | LE | EQ | NE | GE | GT
  | AND
instance Show Binop where
  show ADD = "+"
  show SUB = "-"
  show MUL = "*"
  show DIV = "/"
  show UCParserAST.LT  = "<"
  show LE  = "<="
  show UCParserAST.EQ  = "="
  show NE  = "!="
  show GE  = ">="
  show UCParserAST.GT  = ">"
  show AND = "&&"

data Unop
  = NEG | NOT
instance Show Unop where
  show NEG = "-"
  show NOT = "!"

data Expr
  = CONST   Int
  | VAR     String
  | ARRAY   String Expr
  | ASSIGN  Expr Expr
  | UNARY   Unop Expr
  | BINARY  Binop Expr Expr
  | FUNCALL String [Expr]
  deriving Show
{-
instance Show Expr where
  show CONST i = show i
  show VAR id = show id
  show ARRAY id e = show id ++ "[" ++ show e ++ "]" 
  show ASSIGN lhs rhs = show rhs ++ "=" ++ show lhs
  show UNARY op rhs = show op ++ show rhs
  show BINARY op lhs rhs = show lhs ++ show op ++ show rhs
  show FUNCALL id  = show i
-}
type MaybeExpr = Maybe Expr
type MaybeStmt = Maybe Stmt
data Stmt
  = EMPTY
  | EXPR   Expr
  | IF     Expr Stmt MaybeStmt
  | WHILE  Expr Stmt
  | RETURN MaybeExpr
  | BLOCK  [Stmt]
  deriving Show

data Topdec
  = FUNDEC Type String [Vardec] [Vardec] [Stmt]
  | EXTERN Type String [Vardec]
  | GLOBAL Vardec
  deriving Show

type Program
  = [Topdec]
