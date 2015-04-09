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
import Data.Aeson.Types as AeTypes(Result(..), parse)

import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L 

import GHC.Generics
import Data.Data
import Data.Typeable 
import CCAR.Main.Util
{-- 
	CRUD for Surveys.
	CRUD for survey questions
	CRUD for Responses
	publish surveys.
	publish vote tally. 

--}

data CRUD = Create | Read | Survey_Update | Delete
    deriving(Show, Eq, Read, Data, Generic, Typeable)

data CommandManageSurvey = CommandManageSurvey {
        nickName :: T.Text
        , crudType :: CRUD 
        , survey :: Survey
    } deriving (Show, Eq)




uSurvey = undefined
rSurvey = undefined
dSurvey = undefined 

iSurvey aNickName survey  = do 
            mP <- getBy $ PersonUniqueNickName aNickName
            surveyId <- case mP of 
                    Just (Entity k p) -> insert $ survey {surveyCreatedBy = k}
            return surveyId

insertSurvey aNickName survey  = dbOps $ iSurvey aNickName survey 
updateSurvey aNickName survey  = dbOps $ uSurvey aNickName survey
readSurvey aNick survey = dbOps $ rSurvey nickName survey 
deleteSurvey aNick survey = dbOps $ dSurvey nickName survey 



defaultCountry = insert $ Country "United States" "USA" "US"
defaultGeoLocation = insert $ GeoLocation "Somewhere" 10 10 

defaultIdentificationZone = do 
    country <- defaultCountry
    insert $ IdentificationZone "NJ" "08820" country

process c@(CommandManageSurvey nickName crudType survey) = do 
    case crudType of
        Create -> do 
                    _ <- insertSurvey nickName survey
                    return (GC.Reply, serialize c)
        Survey_Update -> updateSurvey nickName survey
        Read -> readSurvey nickName survey
        Delete -> deleteSurvey nickName survey          


processManageSurvey (Object a ) =
        case (parse parseMS a) of
            Success r ->  process r 
            Error s -> return (GC.Reply, 
                        serialize $ genericErrorCommand $ "Sending message failed " ++ s ++ (show a))



gen (CommandManageSurvey nickName crudType survey) = object ["crudType" .= crudType
                    , "survey" .= survey
                    , "commandType" .= ("ManageSurvey" :: T.Text)
                    , "nickName" .= nickName]

parseMS v = CommandManageSurvey <$>
                    v .: "nickName" <*> 
                    v .: "crudType" <*>
                    v .: "survey"


instance ToJSON CRUD 
instance FromJSON CRUD 
instance ToJSON CommandManageSurvey where
    toJSON  = gen

instance FromJSON CommandManageSurvey where 
    parseJSON (Object v ) = parseMS v 
    parseJSON _           = Appl.empty

