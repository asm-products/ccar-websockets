{-# LANGUAGE TemplateHaskell #-}
module CCAR.Main.EnumeratedTypes where 
import Database.Persist.TH

data SupportedScript = RScript | Stata 
			deriving(Show, Read, Eq)

data SurveyResponse = A | B | C | D | E | F
			deriving(Show, Read, Eq)

data MarketCategory = Consumer | Electronics | Envvironmental | Wholesale 
			deriving (Show, Read, Eq)
data PortfolioSymbolType = Equity | Options | Futures | Currencies | Bonds 
			deriving (Show, Read, Eq)
derivePersistField "SupportedScript"
derivePersistField "SurveyResponse"
derivePersistField "MarketCategory"
derivePersistField "PortfolioSymbolType"
