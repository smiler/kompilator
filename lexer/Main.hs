module Main (main) where

import UCLexer

pretty Id			{str=s}  = "Id " ++ show s
pretty Iconst		{val=v}  = "Iconst " ++ show v
pretty Semicolon	{}  = "Semicolon "
pretty Lparen		{}  = "Lparen    "
pretty Rparen		{}  = "Rparen    "
pretty Lbrace		{}  = "Lbrace    "
pretty Rbrace		{}  = "Rbrace    "
pretty Int			{}  = "Int       "
pretty Void			{}  = "Void      "
pretty Lbrack		{}  = "Lbrack    "
pretty Rbrack		{}  = "Rbrack    "
pretty Comma		{}  = "Comma     "
pretty Plus			{}  = "Plus      "
pretty Minus		{}  = "Minus     "
pretty Times		{}  = "Times     "
pretty Divide		{}	= "Divide    "
pretty Eq			{}  = "Eq        "
pretty Neq			{}  = "Neq       "
pretty Lt			{}  = "Lt        "
pretty Le			{}  = "Le        "
pretty Gt			{}  = "Gt        "
pretty Ge			{}  = "Ge        "
pretty Not			{}  = "Not       "
pretty And			{}  = "And       "
pretty Assign		{}  = "Assign    "
pretty If			{}  = "If        "
pretty Else			{}  = "Else      "
pretty While		{}  = "While     "
pretty Return		{}  = "Return    "
pretty Char			{}  = "Char      "
pretty Error		{msg=m}	= m

main = do
	s <- getContents -- read std input to s
	putStrLn $ concatMap (\s -> pretty s ++ "\n") (ucLexer s)
