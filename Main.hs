module Main where

import Program
import Parser
import Continuations
import LParse

doExec :: String -> String -> String
doExec pro par = run (pFunc parseProgram pro) (\(p,_) -> printRes $ exec p (parsePar par)) (const "ERR")

parsePar :: String -> [Bool]
parsePar [] = []
parsePar ('0':ss) = False:parsePar ss
parsePar ('1':ss) = True:parsePar ss

printRes :: [Bool] -> String
printRes [] = []
printRes (False:xs) = '0' : printRes xs
printRes (True:xs) = '1' : printRes xs

main :: IO ()
main = do
	program <- getLine
	input <- getLine
	putStrLn (doExec program input)
