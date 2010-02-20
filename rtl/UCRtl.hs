module UCRtl where

import UCParserAST as AST
import UCRtlAT as RTL
import Control.Monad.State
import Prelude hiding (init, LT, GT, EQ)

data Symbol
  = FUN  { name :: String, ty :: AST.Type }
  | GLOB { name :: String, ty :: AST.Type }
  | LOC  { name :: String, ty :: AST.Type, temp :: Temp }
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

addSym :: Symbol -> SM ()
addSym sym = modify (\st -> RTLState (tempcount st)
                                     (labelcount st)
                                     (addSym' (syms st)))
  where addSym' [] = [[sym]]
        addSym' ss =
          map (\symbols -> if (sym == head symbols)
                  then sym : symbols
                  else symbols)
              ss

init :: RTLState
init = RTLState (Temp 1) 99 []

parse :: AST.Program -> RTL.Program
parse tree =
  PROGRAM (evalState (mapM topLevel tree) init)

topLevel :: AST.Topdec -> SM RTL.Dec
topLevel (AST.FUNDEC t id args decs body) = do
  l <- newLabel                        -- label
  addSym (FUN id t)
  fs <- mapM localDec args             -- formals
  ls <- mapM localDec decs             -- locals
  let fS = 0                           -- frameSize
  st <- get -- hmm
  is <- liftM join $ mapM funTime body -- insns
  put st
  return (PROC ("P" ++ l) fs ls 0 [])
topLevel (AST.EXTERN t id args) = do
  l <- newLabel
  addSym (FUN id t)
  return (PROC ("P" ++ l) [] [] 0 [])
topLevel (AST.GLOBAL (AST.SCALARDEC t id)) = do
  l <- newLabel
  addSym (GLOB id t)
  return (DATA ("V" ++ l) (case t of CHAR -> sizeof BYTE
                                     INT -> sizeof LONG))
topLevel (AST.GLOBAL (AST.ARRAYDEC t id (Just s))) = do -- hmm?
  l <- newLabel
  addSym (GLOB id t)
  return (DATA ("V" ++ l) (case t of CHAR -> s * sizeof BYTE
                                     INT -> s * sizeof LONG))

localDec :: AST.Vardec -> SM Temp
localDec (SCALARDEC ty id) = do
  t <- newTemp
  addSym (LOC id ty t)
  return t
localDec (ARRAYDEC ty id s) = do -- do what? What, what...
  t <- newTemp
  addSym (LOC id ty t)
  return t

funTime :: AST.Stmt -> SM [Insn]
funTime (AST.EMPTY) = return []  -- nop!
funTime (AST.EXPR e) = return []
funTime (AST.IF e s1 Nothing) = return []
funTime (AST.IF e s1 (Just s2)) = return []
funTime (AST.WHILE e s1) = return []
funTime (AST.RETURN Nothing) = return [JUMP ""]
funTime (AST.RETURN (Just e)) = return [JUMP ""]
funTime (AST.BLOCK stmts) = liftM join $ mapM funTime stmts
