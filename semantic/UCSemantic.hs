module UCSemantic (Symbol(..), ucSemantic) where

import qualified UCParser as Parser
import qualified UCParserAST as AST

data Symbol
  = FUN String AST.Type [Symbol]
  | SCALAR String AST.Type
  | ARRAY String AST.Type
  deriving (Show, Eq)

symName (FUN id _ _) = id
symName (SCALAR id _) = id
symName (ARRAY id _) = id

symType (FUN _ t _) = t
symType (SCALAR _ t) = t
symType (ARRAY _ t) = t

-- used at top level
add :: Symbol -> [[Symbol]] -> [[Symbol]]
add s [] = [[s]]
add s (ss'@(s':_):ss's) =
  if (symName s == symName s') then
      error $ "redeclaration of " ++ symName s
  else
    ss' : add s ss's

-- used in functions
push :: Symbol -> [[Symbol]] -> [[Symbol]]
push s [] = [[s]]
push s (ss'@(s':_):ss's) =
  if (symName s == symName s') then
    (s : ss') : ss's
  else
    ss' : push s ss's

lol st = error $ show st

topLevel :: [AST.Topdec] -> [[Symbol]] -> [[Symbol]]
topLevel [] st = st
topLevel (d:ds) st
  = let st' = case d of
                AST.FUNDEC t id args decs body
                  -- Hur fan gör man saker sekvensiellt på ett snyggt sätt?
                  -> let s = add (FUN id t (funArgs args)) st
                         l = funTimes id body (funDecs decs (funDecs args s))
                     in if (s == l) then s else s -- bara för att tvinga eval av l
                AST.EXTERN t id args
                  -> add (FUN id t (funArgs args)) st
                AST.GLOBAL (AST.SCALARDEC t id)
                  -> add (SCALAR id t) st
                AST.GLOBAL (AST.ARRAYDEC t id _)
                  -> add (ARRAY id t) st
    in topLevel ds st'

funArgs :: [AST.Vardec] -> [Symbol]
funArgs [] = []
funArgs (d:ds) =
  case d of -- återanvänder Symbol för enkelhetens skull
    AST.SCALARDEC t id -> (SCALAR id t) : funArgs ds
    AST.ARRAYDEC t id _ -> (ARRAY id t) : funArgs ds

funDecs :: [AST.Vardec] -> [[Symbol]] -> [[Symbol]]
funDecs [] st = st
funDecs (d:ds) st =
  case d of
    AST.SCALARDEC t id -> funDecs ds (push (SCALAR id t) st)
    AST.ARRAYDEC t id _ -> funDecs ds (push (ARRAY id t) st)

funTimes :: String -> [AST.Stmt] -> [[Symbol]] -> [[Symbol]]
funTimes id [] st = st
funTimes id (s:ss) st =
  case s of
    AST.EMPTY -> funTimes id ss st
--    AST.EXPR e -> do{ exprCheck e st
--               ; funTimes id ss st }
    AST.EXPR e -> return (exprCheck e st) >> funTimes id ss st
    AST.IF e s1 Nothing -> return (exprCheck e st) >> funTimes id ss (stmtCheck s1 st)
    AST.IF e s1 (Just s2) -> return (exprCheck e st) >> funTimes id ss (stmtCheck s2 (stmtCheck s1 st))
    AST.WHILE e s1 -> return (exprCheck e st) >> funTimes id ss (stmtCheck s1 st)
    AST.RETURN Nothing -> funTimes id ss (typeCheck id AST.VOID st)
    -- lolol char = int 
    AST.RETURN (Just e) -> funTimes id ss (typeCheck id (exprCheck e st) st)
    AST.BLOCK s1s -> funTimes id ss (funTimes id s1s st)
---}

exprCheck :: AST.Expr -> [[Symbol]] -> AST.Type
exprCheck _ _ = AST.INT
stmtCheck :: AST.Stmt -> [[Symbol]] -> [[Symbol]]
stmtCheck _ st = st
typeCheck :: String -> AST.Type -> [[Symbol]] -> [[Symbol]]
typeCheck _ _ st = st

ucSemantic = do
  s <- Parser.ucParser
  return (topLevel s [])
