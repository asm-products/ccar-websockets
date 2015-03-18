module Survey where 
import Control.Monad.IO.Class 
import Control.Monad.Logger 
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH 
import CCAR.Main.DBUtils


{-- 
	CRUD for Surveys.
	CRUD for survey questions
	CRUD for Responses
	publish surveys.
	publish vote tally. 

--}

{--
        Survey 
            createdBy PersonId
            createdOn UTCTime
            surveyTitle Text 
            startTime UTCTime
            endTime UTCTime 
            totalVotes Double
            totalCost Double
            maxVotesPerVoter Double
            participantProfile ProfileId 
            expiration UTCTime -- No responses can be accepted after the expiration Date. 

--}



iSurvey aNickName survey = do 
            mP <- getBy $ PersonUniqueNickName aNickName
            case mP of 
                Just (Entity k p) -> insert $ survey {surveyCreatedBy = k}
                    
insertSurvey aNickName survey = dbOps (iSurvey aNickName survey)


