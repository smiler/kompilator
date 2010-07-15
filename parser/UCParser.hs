module UCParser (Vardec(..),Topdec(..),ucParser) where

import UCLexer -- type Token, ucLexer, show
import UCParserAST
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos

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


mytoken :: (Token -> Maybe a) -> GenParser Token () a
mytoken f
  = token show posToken f

scan tok
  = mytoken (\t -> if (t == tok) then Just () else Nothing) <?> show tok

-- Grammar --
program
  = before (many topdec) (scan EOF)

before f g = f >>= \v -> g >>= \_ -> return v
--before f g = do{v <- f; g; return v}
topdec
  = do{ t <- funtype ; id <- ident ; topdec' t id }

topdec' t id
  = do{ glob <- vardec' t id ; return (GLOBAL glob) }
  <|> do{ args <- between (scan Lparen{}) (scan Rparen{}) formals ;
          fundec t id args }
       
vardec
  = do{ t <- typename ; id <- ident ; vardec' t id }

vardec' t id
  = scalardec t id
  <|> do{ size <- between (scan Lbrack{}) (scan Rbrack{}) intconst ; scan Semicolon{} ;
          return (ARRAYDEC t id (Just size)) }

scalardec t id
  = do{ scan Semicolon{} ; return (SCALARDEC t id) }
  

typename
  = do{ scan Int{} ; return INT }
  <|> do{ scan Char{} ; return CHAR } <?> "type"

funtype
  = typename 
  <|> do{ scan Void{} ; return VOID } <?> "type"

fundec t id args
  = do{ scan Semicolon{} ; return (EXTERN t id args) }
  <|> do{ scan Lbrace{} ;
          l <- (many vardec) ; 
          s <- (many stmt) ;
          scan Rbrace{} ;
          return (FUNDEC t id args l s) }

formals
  = do{ scan Void{} ; return [] }
  <|> sepBy1 formaldec (scan Comma{}) <?> "function parameters"

formaldec
  = do{ t <- typename ; id <- ident ; formaldec' t id }

formaldec' t id
  =  do{ scan Lbrack{} ; scan Rbrack{} ;
          return (ARRAYDEC t id Nothing) }
  <|> do{ return (SCALARDEC t id) }

stmt
  =   do{ scan Semicolon{} ;
          return (EMPTY) }
  <|> do{ scan Return{} ; retur }
  <|> do{ scan If{}; exp <- between (scan Lparen{}) (scan Rparen{}) expr ;
          s1 <- stmt ; s2 <- else_part ;
          return (IF exp s1 s2) }
  <|> do{ scan While{} ; e <- between (scan Lparen{}) (scan Rparen{}) expr ;
          s <- stmt ;
          return (WHILE e s) }
  <|> do{ ss <- between (scan Lbrace{}) (scan Rbrace{}) (many stmt) ;
          return (BLOCK ss) }
  <|> do{ e <- expr ; scan Semicolon{} ;
          return (EXPR e) } <?> "statement"

else_part
  = do{ scan Else{} ; s <- stmt ;
        return (Just s) }
  <|> return Nothing

retur
  = do{ scan Semicolon{} ;
        return (RETURN Nothing) }
  <|> do{ s <- expr ; scan Semicolon{} ;
          return (RETURN (Just s)) }

expr
  =  chainr1 expr1 assign <?> "expression"

assign
  = do{ scan Assign{} ; return ASSIGN }

expr1
  = chainl1 expr2 (before (return (BINARY AND)) (scan And{}))

expr2
  = chainl1 expr3 eqop

eqop
 = do{ scan Eq{} ; return (BINARY UCParserAST.EQ) }
 <|> do{ scan Neq{} ; return (BINARY NE) }

expr3
  = chainl1 expr4 relop

relop
  = do{ scan Lt{} ; return (BINARY UCParserAST.LT) }
  <|> do{ scan Le{} ; return (BINARY LE) }
  <|> do{ scan Ge{} ; return (BINARY GE) }
  <|> do{ scan Gt{} ; return (BINARY UCParserAST.GT) }

expr4
  = chainl1 expr5 addop
addop
  = do{ scan Plus{} ; return (BINARY ADD) }
  <|> do{ scan Minus{} ; return (BINARY SUB) }

expr5
  = chainl1 expr6 multop

multop
  = do{ scan Times{} ; return (BINARY MUL) }
  <|> do{ scan Divide{} ; return (BINARY DIV) }

expr6
  = do{ scan Minus{} ; e <- expr6 ; return (UNARY NEG e) }
  <|> do{ scan Not{} ; e <- expr6 ; return (UNARY NOT e) }
  <|> expr7

expr7
  = do{ i <- intconst ; return (CONST i) }
  <|> do{ id <- ident ; vararrfun id}
  <|> between (scan Lparen{}) (scan Rparen{}) expr

vararrfun id
  = do{ e <- between (scan Lbrack{}) (scan Rbrack{}) expr ;
          return (ARRAY id e) }
  <|> do{ args <- between (scan Lparen{}) (scan Rparen{}) (sepBy expr (scan Comma{})) ;
          return (FUNCALL id args) }
  <|> return (VAR id)

intconst
  = mytoken (\t -> case t of Iconst{val=v} -> Just v
                             other -> Nothing) <?> "constant"

ident
  = mytoken (\t -> case t of Id{str=s} -> Just s
                             other -> Nothing) <?> "identifier"

run input
  = do case runParser program () "" (ucLexer input) of
          Left msg -> error $ show msg
          Right a -> a

ucParser s =
 run s
