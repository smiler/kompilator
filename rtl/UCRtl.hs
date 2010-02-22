module UCRtl where

import qualified UCParserAST as AST
import UCRtlAT as RTL
import Control.Monad.State
import Prelude hiding (init, LT, GT, EQ)
import Data.List hiding (init)

data Symbol
  = FUN  { name :: String, ty :: AST.Type, lab :: Label }
  | GLOB { name :: String, ty :: AST.Type, lab :: Label }
  | LOC  { name :: String, ty :: AST.Type, tmp :: Temp }
  -- arrays?
instance Eq Symbol where
  s == s' = (name s) == (name s')
  s /= s' = not (s == s')

data RTLState
  = RTLState { tempcount :: Temp,
               labelcount :: Int,
               syms :: [[Symbol]] }

type SM = State RTLState

newLabel :: SM String 
newLabel = do 
  st <- get
  let l = succ $ labelcount st
  put (RTLState (tempcount st) l (syms st))
  return $ show l

newTemp :: SM Temp
newTemp = do
  st <- get
  let t = succ $ tempcount st
  put (RTLState t (labelcount st) (syms st))
  return t

addSym :: Symbol -> SM Symbol
addSym sym = do
  st <- get
  put (RTLState (tempcount st) (labelcount st) (addSym' (syms st)))
  return sym
  where addSym' [] = [[sym]]
        addSym' (ss@(s : _) : sss) =
          if (sym == s)
            then (sym : ss) : sss
            else ss : addSym' sss

init :: RTLState
init = RTLState (Temp 1) 99 []

parse :: AST.Program -> RTL.Program
parse tree =
  PROGRAM (evalState (mapM topLevel tree) init)

topLevel :: AST.Topdec -> SM RTL.Dec
topLevel (AST.FUNDEC t id args decs body) = do
  l' <- newLabel                       -- label
  let l = "P" ++ l'
  sym <- addSym (FUN id t l)
  fs <- mapM localDec args             -- formals
  ls <- mapM localDec decs             -- locals
  let fS = 0                           -- frameSize
  st <- get -- hmm
  is' <- liftM join $ mapM (funTime sym) body -- insns
  let is = is' ++ [LABDEF ("ret" ++ l)]
  put st
  return (PROC l fs ls 0 is)
topLevel (AST.EXTERN t id args) = do
  l' <- newLabel
  let l = "P" ++ l'
  addSym (FUN id t l)
  fs <- mapM localDec args             -- formals
  return (PROC l fs [] 0 [])
topLevel (AST.GLOBAL (AST.SCALARDEC t id)) = do
  l' <- newLabel
  let l = "V" ++ l'
  addSym (GLOB id t l)
  return (DATA l (case t of AST.CHAR -> sizeof BYTE
                            AST.INT -> sizeof LONG))
topLevel (AST.GLOBAL (AST.ARRAYDEC t id (Just s))) = do -- hmm?
  l' <- newLabel
  let l = "V" ++ l'
  addSym (GLOB id t l)
  return (DATA l (case t of AST.CHAR -> s * sizeof BYTE
                            AST.INT -> s * sizeof LONG))

localDec :: AST.Vardec -> SM Temp
localDec (AST.SCALARDEC ty id) = do
  t <- newTemp
  addSym (LOC id ty t)
  return t
localDec (AST.ARRAYDEC ty id s) = do -- do what? What, what...
  t <- newTemp
  addSym (LOC id ty t)
  return t

funTime :: Symbol -> AST.Stmt -> SM [Insn]
funTime fun (AST.EMPTY) = return []  -- nop!
funTime fun (AST.RETURN Nothing) =
  return [JUMP ("ret" ++ lab fun)]
funTime fun (AST.RETURN (Just e)) = do
  expr <- exprTime e
  return [EVAL rv expr,
          JUMP ("ret" ++ lab fun)]
funTime fun (AST.BLOCK stmts) = liftM join $ mapM (funTime fun) stmts
funTime fun (AST.WHILE e s1) = return []
funTime fun (AST.IF e s1 Nothing) = return []
funTime fun (AST.IF e s1 (Just s2)) = return []
funTime fun (AST.EXPR e) = return []

exprTime :: AST.Expr -> SM Expr
exprTime (AST.CONST i) = return (ICON i)
exprTime (AST.VAR id) = do
  st <- get
  let (Just (sym:_)) = find (\(s:_) -> name s == id) (syms st)
  let r = case sym of
            GLOB {} -> LABREF $ lab sym
            LOC  {} -> TEMP $ tmp sym
  return r
exprTime (AST.ARRAY id s) = return (ICON 0)
exprTime (AST.ASSIGN lhs rhs) = return (ICON 0)
exprTime (AST.UNARY op rhs) = return (ICON 0)
exprTime (AST.BINARY op lhs rhs) = return (ICON 0)
exprTime (AST.FUNCALL id args) = return (ICON 0)
