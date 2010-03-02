module UCRtl where

import qualified UCParserAST as AST
import UCRtlAT as RTL
import Control.Monad.State
import Prelude hiding (LT, GT, EQ,length)
import Data.List hiding (init,length)

data Symbol
  = FUN  { name :: String, ty :: AST.Type, lab :: Label }
  | GLOB { name :: String, ty :: AST.Type, lab :: Label }
  | LOC  { name :: String, ty :: AST.Type, tmp :: Temp }
  | ARR  { name :: String, ty :: AST.Type, addr :: Temp }
  | RARR  { name :: String, ty :: AST.Type, addr :: Temp }
  -- arrays?
instance Eq Symbol where
  s == s' = (name s) == (name s')
  s /= s' = not (s == s')

data RTLState
  = RTLState { tempcount :: Temp,
               labelcount :: Int,
               syms :: [[Symbol]]
               , rtl :: [Insn]
             }

type SM = State RTLState

newLabel :: SM String 
newLabel = do 
  st <- get
  let l = succ $ labelcount st
  put (RTLState (tempcount st) l (syms st) (rtl st))
  return $ show l

newTemp :: SM Temp
newTemp = do
  st <- get
  let t = succ $ tempcount st
  put (RTLState t (labelcount st) (syms st) (rtl st))
  return t

addSym :: Symbol -> SM Symbol
addSym sym = do
  st <- get
  put (RTLState (tempcount st) (labelcount st) (addSym' (syms st)) (rtl st))
  return sym
  where addSym' [] = [[sym]]
        addSym' (ss@(s : _) : sss) =
          if (sym == s)
            then (sym : ss) : sss
            else ss : addSym' sss

add :: [Insn] -> SM [Insn]
add insns = do
  st <- get
--  let is = (revcons insns (rtl st))
  put (RTLState (tempcount st) (labelcount st) (syms st) (rtl st ++ insns))
  return insns
  where revcons [] l = l
        revcons (x:xs) l = revcons xs (x:l)

toss :: SM ()
toss = do
  st <- get
  put (RTLState (tempcount st) (labelcount st) (syms st) [])

initial :: RTLState
initial = RTLState (Temp 1) 99 [] []

typeToSize :: AST.Type -> Int
typeToSize t = case t of
                 AST.CHAR -> sizeof BYTE
                 AST.INT -> sizeof LONG

typeToTy :: AST.Type -> RTL.Ty
typeToTy AST.CHAR = BYTE
typeToTy AST.INT  = LONG

parse :: AST.Program -> RTL.Program
parse tree =
  PROGRAM (evalState (mapM topLevel tree) initial)

safetail [] = []
safetail l = tail l

--topLevel
topLevel :: AST.Topdec -> SM RTL.Dec
topLevel (AST.FUNDEC t id args decs body) = do
  l' <- newLabel                       -- label
  let l = "P" ++ l'
  sym <- addSym (FUN id t l)

  lol <- (get >>= (\st -> return $ tempcount st))
  mapM arguments args             -- formals
  fs <- (get >>= (\st -> return $ tempcount st))
--  frame <- liftM sum $ mapM localDec decs             -- locals
  frame <- localDec decs 0
  mapM (funTime sym) body              -- insns
  ls <- (get >>= (\st -> return $ tempcount st))

  is <- (get >>= (\st-> return ((rtl st) ++ [LABDEF ("ret" ++ l)])))
  toss
  return (PROC l (safetail [lol..fs]) (safetail [fs..ls]) frame is)
topLevel (AST.EXTERN t id args) = do
  l' <- newLabel
  let l = "P" ++ l'
  addSym (FUN id t l)
  lol <- (get >>= (\st -> return $ tempcount st))
  mapM arguments args             -- formals
--  localDec args 0
  fs <- (get >>= (\st -> return $ tempcount st))
  return (PROC l (safetail [lol..fs]) [] 0 [])
topLevel (AST.GLOBAL (AST.SCALARDEC t id)) = do
  l' <- newLabel
  let l = "V" ++ l'
  addSym (GLOB id t l)
  return (DATA l (typeToSize t))
topLevel (AST.GLOBAL (AST.ARRAYDEC t id (Just s))) = do -- hmm?
  l' <- newLabel
  let l = "V" ++ l'
  addSym (GLOB id t l)
  return (DATA l (s * typeToSize t))

--localArgs
arguments :: AST.Vardec -> SM Temp
arguments (AST.SCALARDEC ty id) = do
  t <- newTemp
  addSym (LOC id ty t)
  return t
arguments (AST.ARRAYDEC ty id _) = do -- do what? What, what...
  t <- newTemp
  addSym (RARR id ty t)
  return t

{--localDec
localDec :: AST.Vardec -> SM Int
localDec (AST.SCALARDEC ty id) = do
  t <- newTemp
  addSym (LOC id ty t)
  return 0
localDec (AST.ARRAYDEC ty id (Just s)) = do -- do what? What, what...
  t <- newTemp
  addSym (ARR id ty t)
  return (s * typeToSize ty)
-}
localDec :: [AST.Vardec] -> Int -> SM Int
localDec [] fs = return fs
localDec ((AST.SCALARDEC ty id) : l) fs = do
  t <- newTemp
  addSym (LOC id ty t)
  localDec l fs
localDec ((AST.ARRAYDEC ty id (Just s)) : l) fs = do
  t <- newTemp
  addSym (ARR id ty t)
  add [EVAL t (ICON fs)]
  localDec l (fs + s * typeToSize ty)


--funTime
funTime :: Symbol -> AST.Stmt -> SM ()
funTime fun (AST.EMPTY) = return ()  -- nop!
funTime fun (AST.RETURN Nothing) = do
  add [JUMP ("ret" ++ lab fun)]
--  return [JUMP ("ret" ++ lab fun)]
  return ()
funTime fun (AST.RETURN (Just e)) = do
  (t,_) <- exprTime e
  add [EVAL rv (TEMP t),
       JUMP ("ret" ++ lab fun)]
--  return [EVAL rv (TEMP t), JUMP ("ret" ++ lab fun)]
  return ()
funTime fun (AST.BLOCK stmts) = do
  mapM (funTime fun) stmts
  return ()
funTime fun (AST.WHILE e s1) = do
  check' <- newLabel
  end' <- newLabel
  let check = "L" ++ check'
  let end = "L" ++ end'
  zero <- newTemp
  add [EVAL zero (ICON 0),
       LABDEF check]
  (expr,_) <- exprTime e
  add [CJUMP EQ expr zero end]
  funTime fun s1
  add [JUMP check,
       LABDEF end]
  return ()
funTime fun (AST.IF e s1 s2) = do
  end' <- newLabel
  false' <- newLabel
  let end = "L" ++ end'
  let false = "L" ++ false'
  zero <- newTemp
  (expr,_) <- exprTime e
  add [EVAL zero (ICON 0),
       CJUMP EQ expr zero false]
  funTime fun s1
  add [JUMP end,
       LABDEF false]
  case s2 of
    Nothing -> return ()
    Just s -> funTime fun s
  add [LABDEF end]
  return ()
funTime fun (AST.EXPR e) = do
  exprTime e
  return ()

--exprTime
exprTime :: AST.Expr -> SM (Temp, Ty)
exprTime (AST.CONST i) = do
  ret <- newTemp
  add [EVAL ret (ICON i)]
  return (ret, LONG)
exprTime (AST.VAR id) = do
  st <- get
  let (Just (sym:_)) = find (\(s:_) -> name s == id) (syms st)
  let i = case sym of
            GLOB {} -> LABREF $ lab sym
            LOC  {} -> TEMP $ tmp sym
  ret <- newTemp
  add [EVAL ret i]
  return (ret, typeToTy $ ty sym)
exprTime (AST.ARRAY id ie) = do
  st <- get
  let (Just (sym:_)) = find (\(s:_) -> name s == id) (syms st)
  t0 <- newTemp
  t1 <- newTemp
  t2 <- newTemp
  t3 <- newTemp
  t4 <- newTemp
--  t5 <- newTemp
  ret <- newTemp
  (i,_) <- exprTime ie
  case sym of
    GLOB {} -> add [EVAL t0 (ICON $ typeToSize $ ty sym),
                    EVAL t1 (TEMP i),
                    EVAL t2 (BINARY MUL t0 t1),
                    EVAL t3 (LABREF $ lab sym),
                    EVAL t4 (BINARY ADD t2 t3),
                    EVAL ret (UNARY (LOAD LONG) t4)]
    ARR  {} -> add [EVAL t0 (ICON $ typeToSize $ ty sym),
                    EVAL t1 (TEMP i),
                    EVAL t2 (BINARY MUL t0 t1),
                    EVAL t3 (BINARY ADD t2 (addr sym)),
                    EVAL t4 (BINARY ADD t3 fp),
                    EVAL ret (UNARY (LOAD LONG) t4)]
    RARR {} -> add [EVAL t0 (ICON $ typeToSize $ ty sym),
                    EVAL t1 (TEMP i),
                    EVAL t2 (BINARY MUL t0 t1),
                    EVAL t3 (BINARY ADD t2 (addr sym)), -- ololol offset
                    EVAL t4 (BINARY ADD t3 fp),
                    EVAL ret (UNARY (LOAD LONG) t4)]
  return (ret, typeToTy $ ty sym)
exprTime (AST.ASSIGN lhs rhs) = do
  st <- get
  (rhsVal,_) <- exprTime rhs
  (lhsVal, ty) <- exprTime lhs
  add [STORE ty lhsVal rhsVal]
  return (rhsVal, ty)
exprTime (AST.UNARY op rhs) = do
  t0 <- newTemp
  ret <- newTemp
  (Temp t, ty) <- exprTime rhs
  case op of
    AST.NEG -> do (Temp t, ty) <- exprTime rhs
                  add [EVAL t0 (ICON 0), EVAL ret (BINARY SUB t0 (Temp t))]
                  return (ret, LONG)
    AST.NOT -> exprTime (AST.BINARY AST.EQ (AST.CONST 0) rhs)
--  return (ret, LONG)
exprTime e @ (AST.BINARY op _ _) = do
  ret <- case op of
              AST.ADD -> binary e--add [EVAL ret (BINARY ADD (exprTime lhs) (exprTime rhs))]
              AST.SUB -> binary e--add [EVAL ret (BINARY SUB (exprTime lhs) (exprTime rhs))]
              AST.MUL -> binary e--add [EVAL ret (BINARY MUL (exprTime lhs) (exprTime rhs))]
              AST.DIV -> binary e--add [EVAL ret (BINARY DIV (exprTime lhs) (exprTime rhs))]
              AST.AND -> loland e
              otherwise -> relation e
  return (ret, LONG)
exprTime (AST.FUNCALL id args) = do
  st <- get
  let (Just (f:_)) = find (\(s:_) -> name s == id) (syms st)
  return (Temp 0, LONG) --remember array

--binary 
binary :: AST.Expr -> SM Temp
binary (AST.BINARY op lhs' rhs') = do
  ret <- newTemp
  (lhs,_) <- exprTime lhs'
  (rhs,_) <- exprTime rhs'
  case op of
    AST.ADD -> add [EVAL ret (BINARY ADD lhs rhs)]
    AST.SUB -> add [EVAL ret (BINARY SUB lhs rhs)]
    AST.MUL -> add [EVAL ret (BINARY MUL lhs rhs)]
    AST.DIV -> add [EVAL ret (BINARY DIV lhs rhs)]
  return ret

--relation
relation :: AST.Expr -> SM Temp
relation (AST.BINARY op lhs' rhs') = do
  ret <- newTemp
  trueL <- newLabel >>= (\l -> return ('L' : l))
  end <- newLabel >>= (\l -> return ('L' : l))
  (lhs,_) <- exprTime lhs'
  (rhs,_) <- exprTime rhs'
  case op of
    AST.LT -> add [CJUMP LT lhs rhs trueL]
    AST.LE -> add [CJUMP LE lhs rhs trueL]
    AST.EQ -> add [CJUMP EQ lhs rhs trueL]
    AST.NE -> add [CJUMP NE lhs rhs trueL]
    AST.GE -> add [CJUMP GE lhs rhs trueL]
    AST.GT -> add [CJUMP GT lhs rhs trueL]
  add [EVAL ret (ICON 0),
       JUMP end,
       LABDEF trueL,
       EVAL ret (ICON 1),
       LABDEF end]
  return ret

loland :: AST.Expr -> SM Temp
loland (AST.BINARY AST.AND lhs' rhs') = do
  ret <- newTemp
  zero <- newTemp
  falseL <- newLabel >>= (\l -> return ('L' : l))
  end <- newLabel >>= (\l -> return ('L' : l))
  (lhs,_) <- exprTime lhs'
  (rhs,_) <- exprTime rhs'
  add [EVAL zero (ICON 0),
       CJUMP EQ zero lhs falseL,
       CJUMP EQ zero rhs falseL,
       EVAL ret (ICON 1),
       JUMP end,
       LABDEF falseL,
       EVAL ret (ICON 0),
       LABDEF end]
  return ret