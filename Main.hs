module Main where

import Continuations
import LParse

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
doExec pro par = run (pFunc parseProgram pro) (\(p,_) -> printRes $ exec p (parsePar par)) (const "ERR")

parsePar :: String -> [Bool]
parsePar [] = []
parsePar ('0':ss) = False:parsePar ss
parsePar ('1':ss) = True:parsePar ss

printRes :: [Bool] -> String
printRes [] = []
printRes (False:xs) = '0' : printRes xs
printRes (True:xs) = '1' : printRes xs

parseProgram :: Parser r Program
parseProgram = star parseCommand

parseCommand :: Parser r Command
parseCommand = cParse (not . null) (parseInv <|> parseSwap <|> parsePush <|> parsePop <|> parseNE <|> parseNZ <|> pFail "Expected Command") "Expected Command"

parseInv :: Parser r Command
parseInv = dPrefixParse "!" (constParse Inv)

parseSwap :: Parser r Command
parseSwap = dPrefixParse "_" (constParse Swap)

parsePush :: Parser r Command
parsePush = dPrefixParse "v" (constParse Push)

parsePop :: Parser r Command
parsePop = dPrefixParse "^" (constParse Pop)

parseNE :: Parser r Command
parseNE = dPrefixParse "(" (fmap NE parseProgram <.const.> remCB)
    where remCB = cParse (not . null) (pParse tail noopParse) "Expected ')'"
	
parseNZ :: Parser r Command
parseNZ = dPrefixParse "[" (fmap NZ parseProgram <.const.> remCB)
    where remCB = cParse (not . null) (pParse tail noopParse) "Expected ']'"
	
main :: IO ()
main = do
	program <- getLine
	input <- getLine
	putStrLn (doExec program input)