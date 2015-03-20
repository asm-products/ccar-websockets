{-# LANGUAGE TemplateHaskell #-}
module CCAR.Main.EnumeratedTypes where 
import Database.Persist.TH
import GHC.Generics
import Data.Data
import Data.Typeable 

data SupportedScript = RScript | Stata 
			deriving(Show, Read, Eq)

data SurveyResponse = A | B | C | D | E | F
			deriving(Show, Read, Eq)

data MarketCategory = Consumer | Electronics | Envvironmental | Wholesale 
			deriving (Show, Read, Eq, Data, Generic, Typeable )
data PortfolioSymbolType = Equity | Options | Futures | Currencies | Bonds 
			deriving (Show, Read, Eq, Data, Generic, Typeable)
data MessageDestinationType = Reply | Broadcast 
			deriving (Show, Read, Eq, Data, Generic, Typeable)

data Gender = Male | Female | WontShare -- This needs to address the transgender and other members of our community. 
			deriving (Show, Read, Eq, Data, Generic, Typeable )

{--| Survey prefix is present to allow for other states. |--}
data SurveyPublicationState  = SURVEY_DRAFT | SURVEY_REVIEWED | SURVEY_APPROVED 
			deriving (Show, Read, Eq, Data, Generic, Typeable)



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
derivePersistField "Gender"
derivePersistField "SurveyPublicationState"