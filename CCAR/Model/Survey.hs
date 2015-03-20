module CCAR.Model.Survey 
    (processManageSurvey)
where 
import Control.Monad.IO.Class 
import Control.Monad.Logger 
import Control.Applicative as Appl
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH 
import CCAR.Main.DBUtils
import CCAR.Command.ErrorCommand 
import Data.Text as T 
import qualified CCAR.Main.EnumeratedTypes as Gender 
import qualified CCAR.Main.GroupCommunication as GC
import Data.Aeson
import Data.Aeson.Encode as En
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L 

import GHC.Generics
import Data.Data
import Data.Typeable 

{-- 
	CRUD for Surveys.
	CRUD for survey questions
	CRUD for Responses
	publish surveys.
	publish vote tally. 

--}




iSurvey aNickName survey profile = do 
            mP <- getBy $ PersonUniqueNickName aNickName
            surveyId <- case mP of 
                    Just (Entity k p) -> insert $ survey {surveyCreatedBy = k}
            insert $ profile {profileCreatedFor = surveyId}

insertSurvey aNickName survey profile = dbOps $ iSurvey aNickName survey profile

defInsertSurvey aNickName survey = dbOps $ do
    mP <- getBy $ PersonUniqueNickName aNickName 
    surveyId <- case mP of 
        Just (Entity k p) -> insert $ survey {surveyCreatedBy = k} 
    dId <- defaultIdentificationZone
    dGeo <- defaultGeoLocation
    insert $ Profile surveyId Gender.Male 40 dId



defaultCountry = insert $ Country "United States" "USA" "US"
defaultGeoLocation = insert $ GeoLocation "Somewhere" 10 10 

defaultIdentificationZone = do 
    country <- defaultCountry
    insert $ IdentificationZone "NJ" "08820" country

processManageSurvey (Object a ) = undefined


data CRUD = Create | Read | Update | Delete
    deriving(Show, Eq, Read, Data, Generic, Typeable)

data CommandManageSurvey = CommandManageSurvey {
        crudType :: CRUD 
        , survey :: Survey
    } deriving (Show, Eq)


gen (CommandManageSurvey crudType survey) = object ["crudType" .= crudType
                    , "survey" .= survey
                    , "commandType" .= ("ManageSurvey" :: T.Text)]

parse v = CommandManageSurvey <$> 
                    v .: "crudType" <*>
                    v .: "survey"


instance ToJSON CRUD 
instance FromJSON CRUD 
instance ToJSON CommandManageSurvey where
    toJSON  = gen

instance FromJSON CommandManageSurvey where 
    parseJSON (Object v ) = parse v 
    parseJSON _           = Appl.empty

serialize :: (ToJSON a) => a -> T.Text 
serialize a = L.toStrict $ E.decodeUtf8 $ En.encode a 



