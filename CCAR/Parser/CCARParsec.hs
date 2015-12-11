{--License: license.txt --}
module CCAR.Parser.CCARParsec 
    (readExpr, readExprTree, Stress)
where

import Import
import Data.Text as Text
import CCAR.Model.CcarDataTypes
import CCAR.Model.Maturity
import Control.Monad
syntaxError i = CCARError $ Text.append "Invalid symbol " i 
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_-"


readExprTree :: Text -> [Stress] 
readExprTree input = case parse parseStatements (Text.unpack $ msg $ syntaxError input)
                            (Text.unpack input) of
    Left err -> []
    Right val -> val 

readExpr :: Text -> Value
readExpr input = case parse (parseStatements) (Text.unpack $ msg $ syntaxError input) (Text.unpack input) of 
    Left err ->  toJSON $ syntaxError input
    Right val -> toJSON val



{-- 
    Basis points are usually in integer.
    Percentages can have floating points therefore lets use rational numbers
--}



spaces :: Parser ()
spaces = skipMany1 space


parseNeg :: Parser Sign
parseNeg = do
    char '-'
    return Negative

parsePos :: Parser Sign
parsePos = do
    char '+'
    return Positive

parseSign :: Parser Sign
parseSign = do
    try parseNeg 
    <|> try parsePos
    <|> return Positive
    <?> "Error parsing sign"

parseBasisPoints :: Parser StressValue
parseBasisPoints = do
        string "bps"
        spaces
        sign <- parseSign 
        many space
        pNum <- many1 alphaNum
        return $ BasisPoints sign $ read (pNum)  



parsePercentage :: Parser StressValue
parsePercentage = do
        string "pct"
        spaces
        sign <- parseSign
        many space
        pNum <- many1 alphaNum
        spaces
        many space
        string "%"
        spaces
        pDenom <- liftM read $ many1 digit
        if (pDenom == (0 :: Integer))
            then return $ StressValueError "Divide by zero"
            else return $ Percentage sign $ read (pNum ++ "%" ++ (show pDenom))


parseStressValue :: Parser StressValue
parseStressValue = try parsePercentage 
                <|> try parseBasisPoints 
                <?> "Error parsing stress value"
parseCurrencyStress :: Parser Stress
parseCurrencyStress = do
    string "Create"
    spaces
    string "Currency"
    spaces
    string "Shock"
    spaces
    string "for"
    spaces
    string "major"
    spaces
    curr1 <- many1 alphaNum
    spaces
    string "minor"
    spaces
    curr2 <-many1 alphaNum
    spaces
    stressValue <- parseStressValue
    return $ CurrencyStress  (CurrencyPair (Currency curr1) (Currency curr2)) stressValue


parseEquityStress :: Parser Stress
parseEquityStress = do 
        string "Create"
        spaces
        string "Equity"
        spaces 
        string "Shock"
        spaces
        string "for"
        spaces
        equitySymbol <- many1 alphaNum
        spaces
        stressValue <- parseStressValue
        return $ EquityStress (Equity equitySymbol) stressValue


parseOptionStress :: Parser Stress
parseOptionStress = do
        string "Create"
        spaces
        string "Option"
        spaces
        string "Shock"
        spaces
        string "for"
        spaces
        optionSymbol <- many1 alphaNum
        spaces
        string "Exp"
        spaces
        month <- many1 alphaNum -- Read of month needs to support APR/4 and should be less than 13
        spaces
        year <- many1 alphaNum
        spaces
        string "Strike"
        spaces
        price <- many1 alphaNum
        spaces
        stressValue <- parseStressValue
        return $ OptionStress (CCAROption optionSymbol (Exp (read month) (read year)) (Str $ read price))
                    stressValue


parseTenorValue :: Parser (Mat, StressValue)
parseTenorValue = do
    string "("
    tenorValue <- many1 digit
    tenorPeriod <- many1 alphaNum
    many space
    string "->"
    many space
    stressValue <-  parseStressValue
    string ")"
    return ((createMat tenorValue tenorPeriod), stressValue)
    where
        createMat tenorValue tenorPeriod =
            case tenorPeriod of
                "Y" -> checkBounds (MatY (read tenorValue))
                "M" -> MatM (read tenorValue)
                _ ->   InvalidMaturity

parseTenorCurve :: Parser [(Mat, StressValue)]
parseTenorCurve = do
    string "["
    many space
    tenors <- sepBy parseTenorValue (char ',')
    many space
    string "]" 
    return tenors

parseMaturity :: Parser Mat 
parseMaturity = do
    tenorValue <- many1 digit
    tenorPeriod <- many1 alphaNum
    return $ createMat tenorValue tenorPeriod
    where
    createMat tenorValue tenorPeriod =
        case tenorPeriod of
            "Y" -> MatY (read tenorValue)
            "M" -> MatM (read tenorValue)
            _ ->   InvalidMaturity

parseRatesStress :: Parser Stress
parseRatesStress = do
    string "Create"
    spaces
    string "Rates"
    spaces
    string "Shock"
    spaces
    string "for"
    spaces
    currency <- many1 alphaNum
    many space
    tenors <- parseTenorCurve
    return $ TenorStress (Currency currency) tenors

parseRatesVegaStress :: Parser Stress
parseRatesVegaStress = do
    string "Create"
    spaces
    string "Rates"
    spaces
    string "Vega"
    spaces
    string "Shock"
    spaces
    string "for"
    spaces
    currency <- many1 alphaNum
    spaces
    string "Expiry"
    spaces
    string "="
    spaces
    tenor <- parseMaturity
    many space 
    curve <- parseTenorCurve
    return $ TenorVegaStress (Currency currency) tenor curve

parserError :: Parser Stress 
parserError = do
    return $ StressError $ syntaxError "Unknown error"
parseExpr :: Parser Stress
parseExpr = do 
        try parseEquityStress <|> try parseCurrencyStress
        <|> try parseOptionStress
        <|> try parseRatesStress
        <|> try parseRatesVegaStress
        <|> parserError

parseStatements :: Parser[Stress]
parseStatements = do            
            x <- endBy parseExpr eol
            return x

eol :: Parser String
eol = do 
    try (string ";\n")
    <|> try (string ";")