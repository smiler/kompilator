{
module UCLexer (Token(..), AlexPosn(..), ucLexer) where
}

%wrapper "monad" -- :-/

$digit = 0-9                         -- digits
$alpha = [a-zA-Z]                    -- alphabetic characters
@iconst = $digit+                    -- integer constant
@ident = [\_$alpha][\_$alpha$digit]* -- identifier
@cp = ([^\*]+|\*[^\/]|\n)+           -- inside block comment


tokens :-

  $white+        { ucskip }        -- skip whitespace
  "//"[^\n]*\n   { ucskip }        -- single-line comment
  "/*"           { ucbegin block } -- begin block comment
  <block>@cp     { ucskip }        -- inside block comment
  <block>"*/"    { ucbegin 0 }     -- end block comment
  int            { tok (\p e s -> Int       p e) }
  char           { tok (\p e s -> Char      p e) }
  void           { tok (\p e s -> Void      p e) }
  if             { tok (\p e s -> If        p e) }
  else           { tok (\p e s -> Else      p e) }
  while          { tok (\p e s -> While     p e) }
  return         { tok (\p e s -> Return    p e) }
  \(             { tok (\p e s -> Lparen    p e) }
  \)             { tok (\p e s -> Rparen    p e) } 
  \[             { tok (\p e s -> Lbrack    p e) }
  \]             { tok (\p e s -> Rbrack    p e) }
  \{             { tok (\p e s -> Lbrace    p e) }
  \}             { tok (\p e s -> Rbrace    p e) }
  \,             { tok (\p e s -> Comma     p e) } 
  \+             { tok (\p e s -> Plus      p e) }
  \-             { tok (\p e s -> Minus     p e) }
  \*             { tok (\p e s -> Times     p e) }
  \/             { tok (\p e s -> Divide    p e) }
  \=\=           { tok (\p e s -> Eq        p e) }
  \!\=           { tok (\p e s -> Neq       p e) }
  \<\=           { tok (\p e s -> Le        p e) }
  \>\=           { tok (\p e s -> Ge        p e) }
  \<             { tok (\p e s -> Lt        p e) }
  \>             { tok (\p e s -> Gt        p e) }
  \!             { tok (\p e s -> Not       p e) }
  \&\&           { tok (\p e s -> And       p e) }
  \=             { tok (\p e s -> Assign    p e) }
  \;             { tok (\p e s -> Semicolon p e) }
  "'\n'"         { tok (\p e _ -> Iconst p e (ord '\n')) }
  \'.\'          { tok (\p e [_,c,_] -> Iconst p e (ord c)) }
  @ident         { tok (\p e s -> Id p e s) }
  @iconst        { tok (\p e s -> Iconst p e (read s::Int)) }

{
endPos :: AlexPosn -> Int -> AlexPosn
endPos (AlexPn a l c) len = AlexPn (a+len-1) l (c+len-1)

-- token action, madness
tok:: (AlexPosn -> AlexPosn -> String -> Token) -> 
        (AlexInput -> Int -> Alex Token)
tok f
  = \(pos, _, str) len -> 
          Alex $ \s -> Right (s, (f pos (endPos pos len) (take len str)))


-- The token type:
data Token =
  Id          {pos :: AlexPosn, end :: AlexPosn, str :: String} |
  Iconst      {pos :: AlexPosn, end :: AlexPosn, val :: Int}    |
  Semicolon   {pos :: AlexPosn, end :: AlexPosn}                |
  Lparen      {pos :: AlexPosn, end :: AlexPosn}                |
  Rparen      {pos :: AlexPosn, end :: AlexPosn}                |
  Lbrace      {pos :: AlexPosn, end :: AlexPosn}                |
  Rbrace      {pos :: AlexPosn, end :: AlexPosn}                |
  Int         {pos :: AlexPosn, end :: AlexPosn}                |
  Void        {pos :: AlexPosn, end :: AlexPosn}                |
  Lbrack      {pos :: AlexPosn, end :: AlexPosn}                |
  Rbrack      {pos :: AlexPosn, end :: AlexPosn}                |
  Comma       {pos :: AlexPosn, end :: AlexPosn}                |
  Plus        {pos :: AlexPosn, end :: AlexPosn}                |
  Minus       {pos :: AlexPosn, end :: AlexPosn}                |
  Times       {pos :: AlexPosn, end :: AlexPosn}                |
  Divide      {pos :: AlexPosn, end :: AlexPosn}                |
  Eq          {pos :: AlexPosn, end :: AlexPosn}                |
  Neq         {pos :: AlexPosn, end :: AlexPosn}                |
  Lt          {pos :: AlexPosn, end :: AlexPosn}                |
  Le          {pos :: AlexPosn, end :: AlexPosn}                |
  Gt          {pos :: AlexPosn, end :: AlexPosn}                |
  Ge          {pos :: AlexPosn, end :: AlexPosn}                |
  Not         {pos :: AlexPosn, end :: AlexPosn}                |
  And         {pos :: AlexPosn, end :: AlexPosn}                |
  Assign      {pos :: AlexPosn, end :: AlexPosn}                |
  If          {pos :: AlexPosn, end :: AlexPosn}                |
  Else        {pos :: AlexPosn, end :: AlexPosn}                |
  While       {pos :: AlexPosn, end :: AlexPosn}                |
  Return      {pos :: AlexPosn, end :: AlexPosn}                |
  Block       {pos :: AlexPosn, end :: AlexPosn}                |
  Char        {pos :: AlexPosn, end :: AlexPosn}                |
  Error       {msg :: String}                                   |
  EOF

instance Show Token where
  show Id        {str=s}  = "Id " ++ show s
  show Iconst    {val=v}  = "Iconst " ++ show v
  show Semicolon {} = "Semicolon "
  show Lparen    {} = "Lparen    "
  show Rparen    {} = "Rparen    "
  show Lbrace    {} = "Lbrace    "
  show Rbrace    {} = "Rbrace    "
  show Int       {} = "Int       "
  show Void      {} = "Void      "
  show Lbrack    {} = "Lbrack    "
  show Rbrack    {} = "Rbrack    "
  show Comma     {} = "Comma     "
  show Plus      {} = "Plus      "
  show Minus     {} = "Minus     "
  show Times     {} = "Times     "
  show Divide    {} = "Divide    "
  show Eq        {} = "Eq        "
  show Neq       {} = "Neq       "
  show Lt        {} = "Lt        "
  show Le        {} = "Le        "
  show Gt        {} = "Gt        "
  show Ge        {} = "Ge        "
  show Not       {} = "Not       "
  show And       {} = "And       "
  show Assign    {} = "Assign    "
  show If        {} = "If        "
  show Else      {} = "Else      "
  show While     {} = "While     "
  show Return    {} = "Return    "
  show Char      {} = "Char      "
  show Error     {msg=m} = m
  show EOF       {} = "EOF       "

instance Eq Token where
  Id        {} == Id        {} = True
  Iconst    {} == Iconst    {} = True
  Semicolon {} == Semicolon {} = True
  Lparen    {} == Lparen    {} = True
  Rparen    {} == Rparen    {} = True
  Lbrace    {} == Lbrace    {} = True
  Rbrace    {} == Rbrace    {} = True
  Int       {} == Int       {} = True
  Void      {} == Void      {} = True
  Lbrack    {} == Lbrack    {} = True
  Rbrack    {} == Rbrack    {} = True
  Comma     {} == Comma     {} = True
  Plus      {} == Plus      {} = True
  Minus     {} == Minus     {} = True
  Times     {} == Times     {} = True
  Divide    {} == Divide    {} = True
  Eq        {} == Eq        {} = True
  Neq       {} == Neq       {} = True
  Lt        {} == Lt        {} = True
  Le        {} == Le        {} = True
  Gt        {} == Gt        {} = True
  Ge        {} == Ge        {} = True
  Not       {} == Not       {} = True
  And       {} == And       {} = True
  Assign    {} == Assign    {} = True
  If        {} == If        {} = True
  Else      {} == Else      {} = True
  While     {} == While     {} = True
  Return    {} == Return    {} = True
  Char      {} == Char      {} = True
  EOF       {} == EOF       {} = True
  _ == _ = False
  a /= b = not (a == b)

-- for token building
ucskip input len = ucScan
ucbegin code input len = do alexSetStartCode code; ucScan

-- needed to compile, write own wrapper to fix
alexEOF = alexError "rofl"

-- ugliness
ucEOF :: Alex Token
ucEOF = Alex $ \s -> Right (s, EOF)
ucError :: String -> Alex Token
ucError msg = Alex $ \s -> Right (s, Error{msg=msg})

-- modified version of alexMonadScan
ucScan :: Alex Token
ucScan = do
  inp@(AlexPn _ l c,_,_) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> case sc of
      0 -> ucEOF
      _ -> error $ "parse error on EOF with open block comment at line " ++
                   show l ++ ", col " ++ show c
    AlexError inp'@(AlexPn a l c,chr,s:ss) -> do
      alexSetInput (AlexPn (a+1) l (c+1), s, ss)
      ucError $ "parse error on input " ++ show s
                ++ " at line " ++ show l
                ++ ", col " ++ show c
    AlexSkip inp' len -> do
      alexSetInput inp'
      ucScan
    AlexToken inp' len action -> do
      alexSetInput inp'
      action inp len

-- recursive modified version of runAlex
ucLexer :: String -> [Token]
ucLexer input
  = ucLexer' input ucScan
  where
    ucLexer' input (Alex f)
      = go f (AlexState {alex_pos=alexStartPos,
                         alex_inp = input,
                         alex_chr = '\n',
                         alex_scd = 0})
      where go f s
        = case f s of
--        Right (s', Error{}) -> go f s'             -- filter errors
--        Right (s', Error{msg=m}) -> error m        -- die on error
              Right (s', e@Error{}) -> e : go f s' -- let errors through
              Right (s', EOF) -> [EOF]             -- guess what :O
              Right (s', a) -> a : go f s'         -- lexed a token, continue
}
