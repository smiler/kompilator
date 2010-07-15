module UCSemantic (Symbol(..), ucSemantic) where

import qualified UCParserAST as AST

data Type
  = BASE AST.Type
  | ARRAY AST.Type
  deriving Show	
instance Eq Type where
  BASE AST.INT == BASE AST.CHAR = True
  BASE AST.CHAR == BASE AST.INT = True
  ARRAY AST.INT == ARRAY AST.CHAR = True
  ARRAY AST.CHAR == ARRAY AST.INT = True
  BASE t1 == BASE t2 = t1 == t2
  ARRAY t1 == ARRAY t2 = t1 == t2
  _ == _ = False
  t1 /= t2 = not (t1 == t2)

data Symbol
  = FUN String Type [Symbol]
  | VAR String Type
  deriving (Show, Eq)

data Symbol'
  = Symbol {sym :: Symbol, scope :: Int}
  deriving (Show, Eq)
type Table = [[Symbol']]

symName (FUN id _ _) = id
symName (VAR id _) = id
--symName (ARRAY id _) = id

symType (FUN _ t _) = t
symType (VAR _ t) = t
--symType (ARRAY _ t) = t

getSym :: String -> Table -> Symbol
getSym id [] = error $ "undefined: " ++ id
getSym id ((s':_):ss) =
  if (id == (symName $ sym s')) then
    sym s'
  else
   getSym id ss

getType :: String -> Table -> Type
getType id [] = error $ "undefined: " ++ id
getType id ((s':_):ss) =
  if (id == (symName $ sym s')) then
    symType $ sym s'
  else
    getType id ss

getArgs :: String -> Table -> [Symbol]
getArgs id [] = error $ "undefined: " ++ id
getArgs id ((s':_):ss) =
  if (id == (symName $ sym s')) then
    case sym s' of
      FUN _ _ args -> args
      otherwise -> error $ "call to non-function " ++ show id
  else
   getArgs id ss

-- used at top level
add :: Symbol -> Table -> Table
add s [] = [[Symbol s 0]]
add s (ss'@(s':[]):ss's) =
  if (symName s == (symName $ sym s')) then
      error $ "redeclaration of " ++ symName s
  else
    (ss' : add s ss's)

-- used in functions
push :: Symbol -> Table -> Table 
push s [] = [[Symbol s 1]]
push s (ss'@(s':_):ss's) =
  if (symName s == (symName $ sym s')) then
    if (1 == scope s') then
      error $ "redeclaration of " ++ symName s
    else
      ((Symbol s 1):ss'):ss's
  else
    ss' : push s ss's

--lol (l:ls) = 

topLevel :: [AST.Topdec] -> Table -> Table
topLevel [] st = st
topLevel (d:ds) st =
  let st' = 
        case d of
          AST.FUNDEC t id args decs body
            -> let s = add (FUN id (BASE t) (funArgs args)) st
                   l = funTimes id body (funDecs decs (funDecs args s))
               in if (l == l) then s else s -- force evaluation of l
          AST.EXTERN t id args
            -> add (FUN id (BASE t) (funArgs args)) st
          AST.GLOBAL (AST.SCALARDEC t id)
            -> add (VAR id (BASE t)) st
          AST.GLOBAL (AST.ARRAYDEC t id _)
            -> add (VAR id (ARRAY t)) st
  in topLevel ds st'

funArgs :: [AST.Vardec] -> [Symbol]
funArgs [] = []
funArgs (d:ds) =
  case d of -- återanvänder Symbol för enkelhetens skull
    AST.SCALARDEC t id -> (VAR id (BASE t)) : funArgs ds
    AST.ARRAYDEC t id _ -> (VAR id (ARRAY t)) : funArgs ds

funDecs :: [AST.Vardec] -> Table -> Table
funDecs [] st = st
funDecs (d:ds) st =
  case d of
    AST.SCALARDEC t id -> funDecs ds (push (VAR id (BASE t)) st)
    AST.ARRAYDEC t id _ -> funDecs ds (push (VAR id (ARRAY t)) st)

funTimes :: String -> [AST.Stmt] -> Table -> Table
--funTimes id [] st = error $ show st
funTimes id [] st = st
funTimes id (s:ss) st =
  seq (stmtCheck s id st) (funTimes id ss st)

stmtCheck :: AST.Stmt -> String -> Table -> Table
stmtCheck s id st =
  case s of
    AST.EMPTY
      -> st
    AST.EXPR e
      -> seq (exprCheck e st) st
    AST.IF e s1 Nothing
      -> seq (exprCheck e st) (stmtCheck s1 id st)
    AST.IF e s1 (Just s2)
      -> seq (exprCheck e st) (stmtCheck s2 id (stmtCheck s1 id st))
    AST.WHILE e s1
      -> seq (exprCheck e st) (stmtCheck s1 id st)
    AST.RETURN Nothing
      -> check (getType id st == BASE AST.VOID)
               (st)
               "return type"
    AST.RETURN (Just e)
      -> check (getType id st == (exprCheck e st))
               (st)
               "return type"
    AST.BLOCK stmts
      -> funTimes id stmts st

check :: Bool -> a -> String -> a
check e f m = if e then f else error m

--testId FUN id t st =

exprCheck :: AST.Expr -> Table -> Type
exprCheck (AST.CONST i) st =
  BASE AST.INT
exprCheck (AST.VAR id) st =
  case getType id st of
    BASE t -> BASE t
    otherwise -> error "array variable used as a scalar"
exprCheck (AST.ARRAY id e) st =
  check ((exprCheck e st) == BASE AST.INT)
        (case getType id st of
           ARRAY t -> BASE t
           otherwise -> error "array variable used as a scalar")
        ("non-integer array index")
exprCheck (AST.ASSIGN lhs rhs) st =
  let t = case lhs of
            AST.VAR id
              -> case getSym id st of
                   FUN _ _ _
                     -> error "function used as lvalue in assignment"
                   otherwise
                     -> exprCheck lhs st
            AST.ARRAY _ _
              -> exprCheck lhs st
            otherwise
              -> error "lvalue required as left operand of assignment"
  in check (t == exprCheck rhs st)
           (t)
           ("wrong type in assignment")
exprCheck (AST.UNARY op rhs) st =
  let t = exprCheck rhs st
  in check (t /= BASE AST.VOID)
           (t)
           ("void argument to unary " ++ show op)
exprCheck (AST.BINARY op lhs rhs) st =
  seq (seq ((exprCheck rhs st == BASE AST.VOID) &&
             error "void type in expression")
           ((exprCheck lhs st == BASE AST.VOID) &&
             error "void type in expression"))
      BASE AST.INT
exprCheck (AST.FUNCALL id args) st =
  zipWith' 0 args (getArgs id st)
    where zipWith' :: Int -> [AST.Expr] -> [Symbol] -> Type
          zipWith' n [] [] = getType id st
          zipWith' n (a:as) (na:nas) = seq (argCheck id n a na st)
                                              (zipWith' (n+1) as nas)
          zipWith' n _ _ = error $ "wrong number of arguments to function " ++ id

argCheck id n arg narg st =
  let t1 = symType narg
      t2 = case arg of
            AST.VAR vid -> getType vid st
            otherwise -> exprCheck arg st
  in (t1 == t2) || (error $ "wrong type of argument #" ++ show n ++ " to function " ++ id)
	
ucSemantic p = do
  return (topLevel p [])

