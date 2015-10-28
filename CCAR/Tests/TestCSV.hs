{-# LANGUAGE ForeignFunctionInterface #-}
module CCAR.Tests.TestCSV where 

import Foreign 
import Foreign.C.Types 
import Text.ParserCombinators.Parsec
import Control.Monad.Lift 
import Control.Monad.Lift.Base
import Control.Monad.Lift.IO 
import Control.Monad.Lift.Layer
import Control.Monad.Lift.Top 
import Monad.Abort 
import Monad.ST 
import Monad.RWS 
import Control.Monad.Interface.Try 

-- RWH example.
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n")
quotedCell = 
	do 	char '"'
		content <- many quotedChar 
		char '"' <?> "Incomplete quotes" 
		return content 

-- This function seems like a hack..
-- but i guess that is the only way
-- deal with the boundary.
quotedChar = 
		noneOf "\"" 
	<|> try (string "\"\"" >> return '"')

eol = try (string "\n\r")
	<|> try (string "\r\n")
	<|> string "\n"
	<|> string "\r"
	<?> "End of line"


parseCSV :: String -> Either ParseError [[String]] 
parseCSV input = parse csvFile "Could not parse" input 

foreign import ccall "math.h sin" 
	c_sin :: CDouble -> CDouble

fastsin :: Double -> Double 
fastsin x = realToFrac (c_sin (realToFrac x))

