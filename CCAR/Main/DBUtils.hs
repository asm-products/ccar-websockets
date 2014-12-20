module CCAR.Main.DBUtils where

import Database.Persist
import Database.Persist.Postgresql as DB
import Database.Persist.TH
import Data.Time
import Data.Typeable

import Data.Data

share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
    [persistLowerCase| 
        Person
            firstName String 
            lastName String 
            nickName String 
            password String
            deleted Bool default=False
            PersonUniqueNickName nickName
            deriving Show Eq
        TermsAndConditions
            title String
            description String
            acceptDate UTCTime
            deriving Show Eq 
        CCAR 
            scenarioName String
            scenarioText String
            creator String -- This needs to be the unique name from the person table.
            deleted Bool default=False
            CCARUniqueName scenarioName
            deriving Show Eq
            |]
