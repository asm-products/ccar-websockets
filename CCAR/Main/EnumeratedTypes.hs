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
-- ThreeJS_JSON - invokes JSONLoader for threejs based models.
data SupportedScript = RScript | Stata | Collada | SVG | ThreeJS | ThreeJS_JSON | UnsupportedScriptType
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)



data SurveyResponse = A | B | C | D | E | F
			deriving(Show, Read, Eq, Enum, Bounded, Data, Generic, Typeable)

getSurveyResponses :: [SurveyResponse]
getSurveyResponses = [minBound..maxBound]

{-data MarketCategory = Consumer | Electronics | Envvironmental | Wholesale 
			deriving (Show, Read, Eq, Data, Generic, Typeable )
-}

data PortfolioSymbolType = Equity | Options | Futures | Currencies | Bonds 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)

getPortfolioSymbolTypes :: [PortfolioSymbolType]
getPortfolioSymbolTypes = [minBound..maxBound]


data PortfolioSymbolSide = Buy | Sell | SellShort 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)

getPortfolioSymbolSides :: [PortfolioSymbolSide]
getPortfolioSymbolSides = [minBound..maxBound]

data PortfolioAnalysisResultType = SVGResult | PNGResult
		deriving (Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)

data MessageDestinationType = Reply | Broadcast 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)
getMessageDestinationTypes :: [MessageDestinationType]
getMessageDestinationTypes = [minBound..maxBound]

data Gender = Male | Female | WontShare -- This needs to address the transgender and other members of our community. 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)
getGenderList :: [Gender]
getGenderList = [minBound..maxBound]

{--| Survey prefix is present to allow for other states. |--}
data SurveyPublicationState  = SURVEY_DRAFT | SURVEY_REVIEWED | SURVEY_APPROVED 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)

getSurveyPublicationStates :: [SurveyPublicationState]
getSurveyPublicationStates = [minBound..maxBound]

data RoleType = Guest | ReturningUser | Admin | Support 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)
getRoleTypes = [minBound..maxBound]

data ContactType = Twitter | LinkedIn | Facebook | Phone | Email | Cell | Work | Home | Pinterest
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)

getContactTypes :: [ContactType]
getContactTypes = [minBound..maxBound]

data DocumentFileFormat = Pdf | MicrosoftWord | OpenOffice 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)
getDocumentFileFormats :: [DocumentFileFormat]
getDocumentFileFormats = [minBound..maxBound]			

data ProjectReportType = ClosureReport | ExecutiveSummary | ProjectFinancial
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)
getProjectReportTypes :: [ProjectReportType]
getProjectReportTypes = [minBound..maxBound]

data PublishState = Draft | Review | Published 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)
getPublishStates ::[PublishState]
getPublishStates = [minBound..maxBound]

data MessageCharacteristics = WillAcceptCharges | WillDenyCharges | Undecided
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)
getMessageCharacteristics :: [MessageDestinationType]
getMessageCharacteristics = [minBound..maxBound]
data PermissionType = Read | Write 
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)
getPermissonTypes :: [PermissionType]
getPermissonTypes = [minBound..maxBound]
data TimeUnit = Millis | Seconds | Minutes
			deriving(Show, Read, Enum, Bounded, Eq, Data, Generic, Typeable)


getTimeUnits :: [TimeUnit]
getTimeUnits = [minBound..maxBound]

derivePersistField "SupportedScript"
derivePersistField "SurveyResponse"
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
derivePersistField "PortfolioSymbolSide"
derivePersistField "PortfolioAnalysisResultType"