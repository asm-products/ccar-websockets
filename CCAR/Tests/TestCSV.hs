module CCAR.Tests.TestCSV where 

import Text.ParserCombinators.Parsec

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