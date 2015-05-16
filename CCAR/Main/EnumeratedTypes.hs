{--License: license.txt --}
{-# LANGUAGE TemplateHaskell #-}
module CCAR.Main.EnumeratedTypes where 
import Database.Persist.TH
import GHC.Generics
import Data.Data
import Data.Typeable 
import Data.Text as T



getSupportedScripts :: [SupportedScript]
getSupportedScripts = [minBound..maxBound]

-- We could support a basic script or a file format.
-- Do we need to split these types. 
data SupportedScript = RScript | Stata | Collada | SVG | ThreeJS | UnsupportedScriptType
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)

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

data RoleType = Guest | ReturningUser | Admin | Support 
			deriving (Show, Read, Eq, Data, Generic, Typeable)

data ContactType = Twitter | LinkedIn | Facebook | Phone | Email | Cell | Work | Home 
			deriving (Show, Read, Eq, Data, Generic, Typeable)

data DocumentFileFormat = Pdf | MicrosoftWord | OpenOffice 
			deriving (Show, Read, Eq, Data, Generic, Typeable)

data ProjectReportType = ClosureReport | ExecutiveSummary | ProjectFinancial
			deriving (Show, Read, Eq, Data, Generic, Typeable)

data PublishState = Draft | Review | Published 
			deriving (Show, Read, Eq, Data, Generic, Typeable)

data MessageCharacteristics = WillAcceptCharges | WillDenyCharges | Undecided
			deriving (Show, Read, Eq, Data, Generic, Typeable)
data PermissionType = Read | Write 
			deriving (Show, Read, Eq, Data, Generic, Typeable)
data TimeUnit = Millis | Seconds | Minutes
			deriving (Show, Read, Eq, Data, Generic, Typeable) 

derivePersistField "SupportedScript"
derivePersistField "SurveyResponse"
derivePersistField "MarketCategory"
derivePersistField "PortfolioSymbolType"
derivePersistField "MessageDestinationType"
derivePersistField "MessageCharacteristics"
derivePersistField "Gender"
derivePersistField "SurveyPublicationState"
derivePersistField "RoleType"
derivePersistField "ContactType"
derivePersistField "DocumentFileFormat"
derivePersistField "ProjectReportType"
derivePersistField "PublishState"
derivePersistField "PermissionType"
derivePersistField "TimeUnit"