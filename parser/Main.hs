module Main where

import UCParser

main = do
	s <- ucParser
	putStrLn $ concatMap (\s -> case s of FUNDEC _ id _ _ _ -> show id ++ "{}\n"
                                              EXTERN _ id _ -> show id ++ ";\n"
                                              GLOBAL v -> case v of SCALARDEC _ id -> show id ++ "\n"
                                                                    ARRAYDEC _ id _ -> show id ++ "\n") s
