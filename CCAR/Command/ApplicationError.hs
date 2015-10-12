{--License: license.txt --}
module CCAR.Command.ApplicationError
(ApplicationError(..)
 , appError
)
 where

import GHC.Generics
import Data.Data
import Data.Typeable 
import Data.Text as T
import Data.Aeson
import Control.Applicative as Appl
import Data.HashMap.Lazy as LH (HashMap, lookup, member)

data ApplicationError = ApplicationError {errorCode :: T.Text, message :: T.Text} 
                deriving (Show, Read, Eq, Data, Generic, Typeable)


instance ToJSON ApplicationError 
instance FromJSON ApplicationError


class Error a where 
	appError :: a -> ApplicationError 

instance Error String where
	appError = appErrorString

instance Error Text where 
	appError = appErrorText 

instance Error (LH.HashMap T.Text Value) where 
	appError = parseApplicationError

appErrorString errorMessage = ApplicationError {errorCode = T.pack "Error" 
                                       , message = T.pack errorMessage}

appErrorText errorText = ApplicationError {errorCode = T.pack "Error" 
										, message = errorText}

parseApplicationError value= ApplicationError {errorCode = T.pack "Error"
                               , message = T.pack $ show value}

