module UCRtl where

import qualified UCParserAST as AST
import UCRtlAT as RTL
import Control.Monad.State
import Prelude hiding (LT, GT, EQ,length)
import Data.List hiding (init,length)
import Data.Maybe -- mapMaybe

data Symbol
  = FUN  { name :: String, ty :: AST.Type, lab :: Label }
  | GLOB { name :: String, ty :: AST.Type, lab :: Label }
  | GARR { name :: String, ty :: AST.Type, lab :: Label }
  | LOC  { name :: String, ty :: AST.Type, tmp :: Temp }
  | ARR  { name :: String, ty :: AST.Type, addr :: Temp }
  | RARR  { name :: String, ty :: AST.Type, addr :: Temp }
instance Eq Symbol where
  s == s' = (name s) == (name s')
  s /= s' = not (s == s')

data RTLState
  = RTLState { tempcount :: Temp,
               labelcount :: Int,
               syms :: [[Symbol]],
               rtl :: [Insn]
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
  put (RTLState (tempcount st) (labelcount st) (syms st) (rtl st ++ insns))
  return insns
  where revcons [] l = l
        revcons (x:xs) l = revcons xs (x:l)

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
  PROGRAM (evalState (liftM catMaybes (mapM topLevel tree)) initial)

safetail [] = []
safetail l = tail l

type MaybeDec = Maybe RTL.Dec
--topLevel
topLevel :: AST.Topdec -> SM (Maybe RTL.Dec)
topLevel (AST.FUNDEC t id args decs body) = do
  sym <- addSym (FUN id t id)
  st <- get
  lol <- ( return $ tempcount st)
  mapM arguments args             -- formals
  fs <- (get >>= (return . tempcount))
  frame <- localDec decs 0
  mapM (funTime sym) body              -- insns
  ls <- (get >>= (return . tempcount))
  is <- (get >>= (\st-> return ((rtl st) ++ [LABDEF ("ret" ++ id)])))
  put st -- restore old state (pop symbol table)
  return (Just (PROC id
                     (safetail [lol..fs])
                     (safetail [fs..ls])
                     (((frame - 1) `div` 4 + 1) * 4)
                     is))
topLevel (AST.EXTERN t id args) = do
  addSym (FUN id t id)
  return Nothing
topLevel (AST.GLOBAL (AST.SCALARDEC t id)) = do
  l' <- newLabel
  let l = "V" ++ l'
  addSym (GLOB id t l)
  return (Just (DATA l (typeToSize t)))
topLevel (AST.GLOBAL (AST.ARRAYDEC t id (Just s))) = do -- hmm?
  l' <- newLabel
  let l = "V" ++ l'
  addSym (GARR id t l)
  return (Just (DATA l (s * typeToSize t)))

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
  return ()
funTime fun (AST.RETURN (Just e)) = do
  t <- exprTime e
  add [EVAL rv (TEMP t),
       JUMP ("ret" ++ lab fun)]
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
  expr <- exprTime e
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
  expr <- exprTime e
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

lolTime (AST.VAR id) rhs = do
  st <- get
  let (Just (sym:_)) = find (\(s:_) -> name s == id) (syms st)
  t0 <- newTemp
  case sym of
    GLOB {} -> add [EVAL t0 (LABREF $ lab sym),
                    STORE (typeToTy $ ty sym) t0 rhs]
    LOC  {} -> add [EVAL (tmp sym) (TEMP rhs)]
  return rhs
lolTime (AST.ARRAY id ie) rhs = do
-- fuuuuuuuuuu
  st <- get
  let (Just (sym:_)) = find (\(s:_) -> name s == id) (syms st)
  t0 <- newTemp
  t1 <- newTemp
  t2 <- newTemp
  t4 <- newTemp
  i <- exprTime ie
  add [EVAL t0 (ICON $ typeToSize $ ty sym),
       EVAL t1 (TEMP i),
       EVAL t2 (BINARY MUL t0 t1)]
  case sym of
    GARR {} -> newTemp >>=
                (\t3 -> add [EVAL t3 (LABREF $ lab sym),
                             EVAL t4 (BINARY ADD t2 t3)])
    ARR  {} -> newTemp >>=
                (\t3 ->  add [EVAL t3 (BINARY ADD t2 (addr sym)),
                              EVAL t4 (BINARY ADD t3 fp)])
    RARR {} -> add [EVAL t4 (BINARY ADD t2 (addr sym))]
  add [STORE (typeToTy $ ty sym) t4 rhs]
  return rhs
-- fuuuuuuuuuuuuu

--exprTime
exprTime :: AST.Expr -> SM Temp
exprTime (AST.ASSIGN lhs rhs) = do
  st <- get
  exprTime rhs >>= (lolTime lhs)
exprTime (AST.CONST i) = do
  ret <- newTemp
  add [EVAL ret (ICON i)]
  return ret
exprTime (AST.VAR id) = do
  st <- get
  ret <- newTemp
  let (Just (sym:_)) = find (\(s:_) -> name s == id) (syms st)
  case sym of
    GLOB {} -> newTemp >>=
                (\t0 ->
                   add [EVAL t0 (LABREF $ lab sym),
                        EVAL ret (UNARY (LOAD $ typeToTy $ ty sym) t0)
                       ]
                )
    LOC  {} -> add [EVAL ret (TEMP $ tmp sym)]
  return ret
exprTime (AST.ARRAY id ie) = do
  st <- get
  let (Just (sym:_)) = find (\(s:_) -> name s == id) (syms st)
  t0 <- newTemp
  t1 <- newTemp
  t2 <- newTemp
  t3 <- newTemp
  ret <- newTemp
  i <- exprTime ie
  add [EVAL t0 (ICON $ typeToSize $ ty sym),
       EVAL t1 (TEMP i),
       EVAL t2 (BINARY MUL t0 t1)]
  case sym of
    GARR {} -> newTemp >>=
                (\t4 -> add [EVAL t4 (LABREF $ lab sym),
                             EVAL t3 (BINARY ADD t2 t4)])
    ARR  {} -> newTemp >>=
                (\t4 -> add [EVAL t4 (BINARY ADD t2 (addr sym)),
                             EVAL t3 (BINARY ADD t4 fp)])
    RARR {} -> add [EVAL t3 (BINARY ADD t2 (addr sym))]
                    
  add [EVAL ret (UNARY (LOAD (typeToTy $ ty sym)) t3)]
  return ret
exprTime (AST.UNARY op rhs) = do
  t0 <- newTemp
  case op of
    AST.NEG -> do t <- exprTime rhs
                  add [EVAL t0 (ICON 0), EVAL rv (BINARY SUB t0 t)]
                  return rv
    AST.NOT -> exprTime (AST.BINARY AST.EQ (AST.CONST 0) rhs)
exprTime e @ (AST.BINARY op _ _) = do
  ret <- case op of
           AST.ADD -> binary e
           AST.SUB -> binary e
           AST.MUL -> binary e
           AST.DIV -> binary e
           AST.AND -> loland e
           otherwise -> relation e
  return ret
exprTime (AST.FUNCALL id args) = do
  st <- get
  let (Just (f:_)) = find (\(s:_) -> name s == id) (syms st)
  l <- mapM callarg args
  add [CALL Nothing (lab f) l] -- lol? probably best to ignore
  return rv

callarg :: AST.Expr -> SM Temp
callarg e @ (AST.VAR id) = do
  st <- get
  ret <- newTemp
  let (Just (sym:_)) = find (\(s:_) -> name s == id) (syms st)
  case sym of
    GLOB {} -> newTemp >>= 
                 (\t0 -> add [EVAL t0 (LABREF $ lab sym),
                              EVAL ret (UNARY (LOAD $ typeToTy $ ty sym) t0)] >>
                           return ret)
    GARR {} -> add [EVAL ret (LABREF $ lab sym)] >> return ret
    RARR {} -> return $ addr sym
    ARR  {} -> add [EVAL ret (BINARY ADD (addr sym) fp)] >> return ret
    LOC  {} -> exprTime e
callarg e = do
  exprTime e

--binary 
binary :: AST.Expr -> SM Temp
binary (AST.BINARY op lhs' rhs') = do
  lhs <- exprTime lhs'
  rhs <- exprTime rhs'
  ret <- newTemp
  case op of
    AST.ADD -> add [EVAL ret (BINARY ADD lhs rhs)]
    AST.SUB -> add [EVAL ret (BINARY SUB lhs rhs)]
    AST.MUL -> add [EVAL ret (BINARY MUL lhs rhs)]
    AST.DIV -> add [EVAL ret (BINARY DIV lhs rhs)]
  return ret

--relation
relation :: AST.Expr -> SM Temp
relation (AST.BINARY op lhs' rhs') = do
  trueL <- newLabel >>= (\l -> return ('L' : l))
  end <- newLabel >>= (\l -> return ('L' : l))
  lhs <- exprTime lhs'
  rhs <- exprTime rhs'
  ret <- newTemp
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
  zero <- newTemp
  falseL <- newLabel >>= (\l -> return ('L' : l))
  end <- newLabel >>= (\l -> return ('L' : l))
  lhs <- exprTime lhs'
  rhs <- exprTime rhs'
  ret <- newTemp
  add [EVAL zero (ICON 0),
       CJUMP EQ zero lhs falseL,
       CJUMP EQ zero rhs falseL,
       EVAL ret (ICON 1),
       JUMP end,
       LABDEF falseL,
       EVAL ret (ICON 0),
       LABDEF end]
  return ret
