{-# LANGUAGE TemplateHaskell #-}

module CCAR.Main.DBUtils where


import Database.Persist
import Database.Persist.Postgresql as DB
import Database.Persist.TH
import Data.Time
import Data.Typeable
import Data.Text
import Data.Data
import CCAR.Main.EnumeratedTypes 


type NickName = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
    [persistLowerCase| 
        Person
            firstName Text 
            lastName Text 
            nickName NickName
            password Text
            lastLoginTime UTCTime default=CURRENT_TIMESTAMP
            deleted Bool default=False
            PersonUniqueNickName nickName
            deriving Show Eq
        TermsAndConditions
            title Text
            description Text
            acceptDate UTCTime
            deriving Show Eq 
        CCAR 
            scenarioName Text
            scenarioText Text
            creator Text -- This needs to be the unique name from the person table.
            deleted Bool default=False
            CCARUniqueName scenarioName
            deriving Show Eq
        Messages 
            from NickName 
            to NickName 
            message Text 
            messageSentTime UTCTime 
            messageReadTime UTCTime
            deriving Show Eq
        Workbench
            name Text
            scriptType SupportedScript
            script Text 
            lastModified UTCTime default=CURRENT_TIMESTAMP
            ownerId PersonId 
            deriving Show Eq
        Wallet 
            name Text 
            passphrase Text 
            publicAddress Text 
            lastModified UTCTime default=CURRENT_TIMESTAMP
            deriving Show Eq 
        Gift 
            from NickName 
            to NickName
            message Text 
            sentDate UTCTime
            acceptedDate UTCTime 
            rejectDate UTCTime -- if the receiver doesnt want the gift. 
            amount Double  -- not the best type. But all amounts are in SWBench.
            deriving Show Eq 
        Reputation 
            amount Double
            ownerId PersonId 
            deriving Show Eq
        Survey 
            totalCost Double
            createdBy PersonId
            createdOn UTCTime
            expiration UTCTime -- No responses can be accepted after the expiration Date. 
            deriving Show Eq
        SurveyQuestion
            surveyId SurveyId 
            question Text 
            questionResearch Text -- All the relevant survey, disclaimers etc.
            deriving Show Eq
        Response
            surveyQuestionId 
            responderId PersonId -- This wont last, as we want to maintain anonymity and integrity
            response SurveyResponse
            responseComments Text
            surveyCostLimit Double -- a function that decides how many times a responder can vote 
            deriving Show Eq

            |]
