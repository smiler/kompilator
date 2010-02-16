module UCRtl where

import UCParserAST as AST
import UCRtlAT as ABS
import Control.Monad.State as ST
import Prelude hiding (init, LT, GT, EQ)

--type RTL = State RTLState Program
data RTLState = RTLState { fp :: Int, rv :: Int,
                           temp :: Int, label :: Int }

type RTL = State RTLState
--globalState :: STRef RTLState
--globalState = newSTRef (newIORef init)

init :: RTLState
init = RTLState 0 1 1 99

makeLabel = "L100"

parse :: [AST.Topdec] -> RTL ABS.Program
--parse (AST.FUNDEC t id args decs body:tds) = do
--  st <- get
--parse (AST.EXTERN t id args:tds) = do
--  st <- get
parse (AST.GLOBAL (AST.SCALARDEC t id):tds) = do
  st <- get
  l <- makeLabel
  return (DATA l (case t of CHAR -> sizeof BYTE ; INT -> sizeof LONG))
--parse (AST.GLOBAL (AST.ARRAYDEC t id _):tds) = do
--  st <- get


runRTL :: RTL ABS.Program
runRTL = do
  st <- init
  evalState st parse
