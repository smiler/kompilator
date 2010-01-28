module Main (main) where

import UCLexer

main = do
	s <- getContents -- read std input to s
	putStrLn $ concatMap (\s -> show s ++ "\n") (ucLexer s)
