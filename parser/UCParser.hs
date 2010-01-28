module UCParser (Vardec(..),Topdec(..),ucParser) where

import UCLexer -- type Token, ucLexer, show
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
{-
tok :: Token -> GenParser Token st Token
tok x
	= tokenPrim show nextPos testToken
	where
		show t = show t
		nextPos pos t toks = pos
		testToken t@(Id p e s) = if(t == x) then Just t else Nothing
		testToken t@(Iconst p e v) = Just t
		testToken t@( n p e) = if (n ==(name x)) then Just t else Nothing
		testToken _ = Nothing
-}
posToken Id        {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Iconst    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Semicolon {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Lparen    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Rparen    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Lbrace    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Rbrace    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Int       {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Void      {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Lbrack    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Rbrack    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Comma     {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Plus      {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Minus     {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Times     {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Divide    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Eq        {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Neq       {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Lt        {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Le        {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Gt        {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Ge        {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Not       {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken And       {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Assign    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken If        {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Else      {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken While     {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Return    {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken Char      {pos=(AlexPn _ l c)} = newPos "stdin" l c
posToken _ = newPos "fail" 0 0
--posToken Error     {pos=(AlexPn c _ l)} = newPos "stdin" l c

data Type
  = INT
  | CHAR
  | VOID

type MaybeInt = Maybe Int

data Vardec
  = SCALARDEC Type String
  | ARRAYDEC  Type String MaybeInt

type MaybeStmt = Maybe Stmt

data Binop
  = ADD | SUB | MUL | DIV
  | LT | LE | EQ | NE | GE | GT
  | ANDALSO

data Unop
  = NEG | NOT

data Expr
  = CONST   Int
  | VAR     String
  | ARRAY   String Int
  | ASSIGN  Expr Expr
  | UNARY   Unop Expr
  | BINARY  Binop Expr Expr
  | FUNCALL String [Expr]

data Stmt
  = EMPTY
  | EXPR   Expr
  | IF     Expr Stmt Stmt
  | WHILE  Expr Stmt
  | RETURN MaybeStmt
  | BLOCK  [Stmt]

data Topdec
  = FUNDEC Type String [Vardec] [Vardec] Stmt
  | EXTERN Type String [Vardec]
  | GLOBAL Vardec

type Program
  = [Topdec]

mytoken :: (Token -> Maybe a) -> GenParser Token () a
mytoken f
  = token show posToken f

scan tok
  = mytoken (\t -> if (t == tok) then Just () else Nothing) <?> show tok

-- Grammar --
program
  = many topdec

before f g = f >>= \v -> g >>= \_ -> return v

topdec
--  = vardec >>= \v -> scan Semicolon{} >>= \_ -> return v
-- = before vardec (scan Semicolon{})
  = do{ v <- try vardec ;
        return (GLOBAL v) }
  <|> do{ ft <- funtype ; id <- ident ;
      scan Lparen{} ; args <- formals ; scan Rparen{} ;
      body <- funbody ;
      case body of Just (l, b) -> return (FUNDEC ft id args l b)
                   Nothing -> return (EXTERN ft id args) }

vardec
  = try $ before scalardec (scan Semicolon{})
  <|> before arraydec (scan Semicolon{})

scalardec
  = do{ t <- typename ; id <- ident ; return (SCALARDEC t id) }

arraydec
  = do{ t <- typename ; id <- ident ;
      scan Lbrack{} ; size <- intconst ; scan Rbrack{} ;
      return (ARRAYDEC t id (Just size)) }

--typename
--  = 
typename
  = mytoken (\t -> case t of
                      Int{} -> Just INT
                      Char{} -> Just CHAR
                      other -> Nothing)
funtype
  = typename <|> mytoken (\t -> case t of Void{} -> Just VOID
                                          other -> Nothing)
funbody
  = do{ scan Lbrace{} ; l <- locals ; s <- stmts ; scan Rbrace{} ;
        return (Just (l, BLOCK s))}
  <|> do{ scan Semicolon{} ; return Nothing }

formals
  = do{ scan Void{} ; return [] }
  <|> sepBy1 formaldec (scan Comma{})

formaldec
  = try scalardec
  <|> do{ t <- typename ; id <- ident ; scan Lbrack{} ; scan Rbrack{} ;
          return (ARRAYDEC t id Nothing) }

locals
  = sepEndBy vardec (scan Semicolon{})

stmts
  = sepEndBy stmt (scan Semicolon{})

stmt
  = mytoken (\_ -> Just EMPTY)

intconst
  = mytoken (\t -> case t of Iconst{val=v} -> Just v
                             other -> Nothing)
--  = mytoken (val >>= (\_ -> Just))
--  = mytoken (>>= (Just . val))

ident
  = mytoken (\t -> case t of Id{str=s} -> Just s
                             other -> Nothing)

run input
  = do case runParser program () "" (ucLexer input) of
          Left msg -> error $ show msg
          Right a -> a

ucParser =  do
  s <- getContents
  return (run s)