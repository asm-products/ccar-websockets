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
import System.Environment(getEnv)
import Data.ByteString as DBS hiding (putStrLn)
import Data.ByteString.Char8 as C8 hiding(putStrLn) 

type NickName = Text


getPoolSize :: IO Int 
getPoolSize = return 10

getConnectionString :: IO ByteString 
getConnectionString = do
        host <- getEnv("PGHOST")
        dbName <- getEnv("PGDATABASE")
        user <- getEnv("PGUSER")
        pass <- getEnv("PGPASS")
        port <- getEnv("PGPORT")
        return $ C8.pack ("host=" ++ host
                    ++ " "
                    ++ "dbname=" ++ dbName
                    ++ " "
                    ++ "user=" ++ user 
                    ++ " " 
                    ++ "password=" ++ pass 
                    ++ " " 
                    ++ "port=" ++ port)


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
        Country 
            name Text 
            iso_3 Text
            iso_2 Text
            deriving Show Eq
        Language 
            name Text 
            font Text 
            country CountryId 
            deriving Show Eq
        -- Could be the postal zone,
        -- Geographic zone etc.
        -- typical entries: 
        -- NY 12345
        -- NJ 22334 something like so.
        IdentificationZone 
            zoneName Text
            zoneType Text
            country CountryId 
            deriving Eq Show
        GeoLocation 
            latitude Double -- most likely in radians.
            longitude Double
            deriving Eq Show 
        Preferences 
            preferencesFor PersonId 
            maxHistoryCount Int default = 400 -- Maximum number of messages in history
            deriving Eq Show 
        Profile 
            createdFor PersonId 
            gender Text 
            age Double
            identificationZone IdentificationZoneId
            geoLocation GeoLocation
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
        MessageP -- Persistent version of messages. This table is only for general messages and private messages.
                 -- MessageDestinationType is mainly, private message or broadcast.
                 -- Group messages will be handled as part of group messages.
                 -- Bite me??
            from NickName 
            to NickName 
            message Text
            iReadIt MessageCharacteristics
            destination MessageDestinationType
            deriving Show Eq
        Workbench
            name Text
            scriptType SupportedScript
            script Text 
            lastModified UTCTime default=CURRENT_TIMESTAMP
            ownerId PersonId 
            deriving Show Eq
        WorkbenchGroup
            workbenchId WorkbenchId 
            personId PersonId -- List of users who share a workbench with reod only comments
            deriving Show Eq 
        WorkbenchComments 
            workbenchId 
            comment Text 
            commenter PersonId 
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
            createdBy PersonId
            createdOn UTCTime
            startTime UTCTime
            endTime UTCTime 
            totalVotes Double
            totalCost Double
            maxVotesPerVoter Double
            participantProfile ProfileId 
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
        Marketplace 
            description Text 
            creator PersonId 
            coverCharge Double -- As a means to establish trust 
            category MarketCategory 
            deriving Show Eq 
        Product 
            description Text 
            creator PersonId 
            cost Double 
            unitOfMeasure Text 
            defaultImageUrl Text 
            deriving Show Eq 
        ProductImage
            productId ProductId 
            imageUrl Text 
            deriving Show Eq 
        ProductDiscount 
            productId ProductId 
            discountAmount Double -- number between 0 - 100 
            startDate UTCTime 
            endDate UTCTime 
            deriving Show Eq 
        PassphraseManager 
            passphrase Text 
            passphraseKey Text 
            deriving Show Eq
        Portfolio 
            symbol Text
            quantity Double
            symbolType PortfolioSymbolType 
            deriving Show Eq
        MarketDataSubscription 
            ownerId PersonId
            sourceName Text 
            realtimeInterval Double  
            deriving Show Eq 
        |]
