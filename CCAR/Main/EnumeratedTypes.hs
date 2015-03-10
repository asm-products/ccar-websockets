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
data MessageDestinationType = Reply | Broadcast 
			deriving (Show, Read, Eq)

-- The good old way to prevent spammers.
-- A user that sent messages that 
-- the senders deny will be banned or 
-- their privelege to send private messages will 
-- be revoked.
-- It can happen that a set of users could collude to
-- game the system. Each user will have to 
-- to gain some reputation before they can start to 
-- send private messages.
data MessageCharacteristics = WillAcceptCharges | WillDenyCharges | Undecided
			deriving (Show, Read, Eq)

derivePersistField "SupportedScript"
derivePersistField "SurveyResponse"
derivePersistField "MarketCategory"
derivePersistField "PortfolioSymbolType"
derivePersistField "MessageDestinationType"
derivePersistField "MessageCharacteristics"