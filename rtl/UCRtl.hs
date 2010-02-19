module UCRtl where

import UCParserAST as AST
import UCRtlAT as RTL hiding (label)
import Control.Monad.State
import Prelude hiding (init, LT, GT, EQ)

--type RTL = State RTLState Program
data RTLState = RTLState { temp :: Int, label :: Int }

--instance Monad (State RTLState)

--type RTLState = State (Int, Int)
--newtype RTLState = State { temp :: Int, label :: Int }

init :: RTLState
init = RTLState 1 99

newLabel :: RTLState -> (RTLState, String)
newLabel st =
  let l = succ $ label st
  in (RTLState (temp st) l, "L" ++ show l)

newTemp :: RTLState -> (RTLState, Int)
newTemp st =
  let t = succ $ temp st
  in (RTLState t (label st), t)

topLevel :: RTLState -> [AST.Topdec] -> [RTL.Dec]
topLevel st [] = []
topLevel st (AST.FUNDEC t id args decs body : tds) =
  topLevel st tds
topLevel st (AST.EXTERN t id args : tds) =
  topLevel st tds
topLevel st (AST.GLOBAL (AST.SCALARDEC t id) : tds) =
  let (st', l) = newLabel st
      dec = DATA l (case t of 
                      CHAR -> sizeof BYTE
                      INT -> sizeof LONG)
  in dec : topLevel st' tds
topLevel st (AST.GLOBAL (AST.ARRAYDEC t id (Just s)) : tds) =
  let (st', l) = newLabel st
      dec = DATA l (case t of
                      CHAR -> s * sizeof BYTE
                      INT -> s * sizeof LONG)
  in dec : topLevel st' tds


parse :: AST.Program -> RTL.Program
parse tree =
  PROGRAM (topLevel init tree)
