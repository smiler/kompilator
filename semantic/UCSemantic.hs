module UCSemantic (buildSymTable, SymData(..), ucSemantic) where

import qualified UCParser as Parser
import qualified UCParserAST as AST


data SymData
  = FUNDEC String AST.Type [AST.Vardec] -- Function declaration
  | EXTERN String AST.Type [AST.Vardec] -- Extern function declaration
  | GLOBAL String AST.Vardec -- Global variable declaration
  deriving (Show)

getIdFromSym (FUNDEC id _ _) = id
getIdFromSym (EXTERN id _ _) = id
getIdFromSym (GLOBAL id _) = id

-- Add symbol to symbol table (replace with pushSym below)
updateSym sym s = (s : sym)

-- Push symbol to symbol table
pushSym :: [[SymData]] -> SymData -> [[SymData]]
pushSym [] sym = [[sym]]
pushSym ((l @ x:xs):syms) sym =
  if (id(l) == id(sym)) then 
    [(sym : [x]) ++ xs] ++ syms
  else 
    [l] : pushSym syms sym
      where id = getIdFromSym

-- Build symbol table
buildSymTable [] sym     = sym
buildSymTable (x:xs) sym =
  case x of 
    AST.FUNDEC t id args _ _ 
      -> buildSymTable xs (updateSym sym (FUNDEC id t args))
    AST.EXTERN t id args
      -> buildSymTable xs (updateSym sym (EXTERN id t args))
    AST.GLOBAL t
      -> buildSymTable xs (updateSym sym (GLOBAL (id t) t))
        where
          id = AST.getIdFromVardec

-- Validate symbol table
-- Todo: Do something          
checkSymTable xs = True

ucSemantic = do
  s <- Parser.ucParser
  return (buildSymTable s [])
