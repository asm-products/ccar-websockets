{--License: license.txt --}
module CCAR.Command.ErrorCommand
(ErrorCommand(..)
 , genericErrorCommand 
 , parseErrorCommand
 , genErrorCommand
 , genericErrorCommandText
)
 where

import GHC.Generics
import Data.Data
import Data.Typeable 
import Data.Text as T
import Data.Aeson
import Control.Applicative as Appl

data ErrorCommand = ErrorCommand {errorCode :: T.Text, message :: T.Text} 
                deriving (Show, Eq)


genericErrorCommand errorMessage = ErrorCommand {errorCode = T.pack "Error" 
                                       , message = T.pack errorMessage}

genericErrorCommandText errorText = ErrorCommand {errorCode = T.pack "Error" 
										, message = errorText}

instance ToJSON ErrorCommand where
    toJSON = genErrorCommand

instance FromJSON ErrorCommand where
    parseJSON (Object v) = parseErrorCommand v
    parseJSON _          = Appl.empty


genErrorCommand (ErrorCommand e  m) = object ["errorCode" .= e
                                              , "message" .= m]

parseErrorCommand value= do
        return $ ErrorCommand {errorCode = T.pack "Error"
                               , message = T.pack $ show value}

