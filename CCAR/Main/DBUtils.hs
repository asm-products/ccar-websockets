module CCAR.Main.DBUtils where

import Database.Persist
import Database.Persist.Postgresql as DB
import Database.Persist.TH
import Data.Time
import Data.Typeable
import Data.Text
import Data.Data

share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
    [persistLowerCase| 
        Person
            firstName Text 
            lastName Text 
            nickName Text 
            password Text
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
            |]
