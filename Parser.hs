module Parser(parseProgram) where

import LParse
import Program

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
