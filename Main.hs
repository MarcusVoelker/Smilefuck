module Main where

import Continuations

data Command = Inv | Swap | Push | Pop | NE [Command] | NZ [Command]

type Program = [Command]

type Config = (Program,[Bool],[Bool],Bool)

instance Show Command where
  show Inv = "!"
  show Swap = "_"
  show Push = "v"
  show Pop = "^"
  show (NE p) = "(" ++ (flatten $ map show p) ++ ")"
  show (NZ p) = "[" ++ (flatten $ map show p) ++ "]"

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

step :: Config -> Config
step (Inv:p,l,r,a) = (p,l,r,not a)
step (Swap:p,l,r,a) = (p,r,l,a)
step (Push:p,l,r,a) = (p,a:l,r,a)
step (Pop:p,(a:l),r,_) = (p,l,r,a)
step ((NE i):p,[],r,a) = (p,[],r,a)
step ((NE i):p,l,r,a) = (\(l',r',a') -> step ((NE i):p,l',r',a')) $ simulate i (l,r,a)
step ((NZ i):p,l,r,False) = (p,l,r,False)
step ((NZ i):p,l,r,a) = (\(l',r',a') -> step ((NZ i):p,l',r',a')) $ simulate i (l,r,a)

simulate :: Program -> ([Bool],[Bool],Bool) -> ([Bool],[Bool],Bool)
simulate [] c = c
simulate p (l,r,a) = (\(p,l,r,a) -> simulate p (l,r,a)) $ step (p,l,r,a)

exec :: Program -> [Bool] -> [Bool]
exec p b = (\(_,r,_) -> r) $ simulate p (b,[],False)

doExec :: String -> String -> String
doExec pro par = run (parseProgram pro) (\(p,_) -> printRes $ exec p (parsePar par)) (const "ERR")

parsePar :: String -> [Bool]
parsePar [] = []
parsePar ('0':ss) = False:parsePar ss
parsePar ('1':ss) = True:parsePar ss

printRes :: [Bool] -> String
printRes [] = []
printRes (False:xs) = '0' : printRes xs
printRes (True:xs) = '1' : printRes xs

parseCommand :: String -> DCont r String (Command,String)
parseCommand ('!':ss) = return (Inv,ss)
parseCommand ('_':ss) = return (Swap,ss)
parseCommand ('v':ss) = return (Push,ss)
parseCommand ('^':ss) = return (Pop,ss)
parseCommand ('(':ss) = parseNE ('(':ss)
parseCommand ('[':ss) = parseNZ ('[':ss)
parseCommand ss = throw "Expected Command!"

parseProgram :: String -> DCont r String (Program,String)
parseProgram [] = return ([],[])
parseProgram s@(')':ss) = return ([],s)
parseProgram s@(']':ss) = return ([],s)
parseProgram ss = do
	(c,r) <- parseCommand ss
	(p,r') <- parseProgram r
	return (c:p,r')
	
parseNE :: String -> DCont r String (Command,String)
parseNE ('(':ss) = do
	(p,r) <- parseProgram ss
	if (head r == ')') then return (NE p,tail r) else throw "Expected ')'"
	
parseNZ :: String -> DCont r String (Command,String)
parseNZ ('[':ss) = do
	(p,r) <- parseProgram ss
	if (head r == ']') then return (NZ p,tail r) else throw "Expected ']'"
	
main :: IO ()
main = do
	program <- getLine
	input <- getLine
	putStrLn (doExec program input)