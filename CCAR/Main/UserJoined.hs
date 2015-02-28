module CCAR.Main.UserJoined 
	(UserJoined 
	 , parseUserJoined
	 , genUserJoined)
where 

import Data.Text as T  hiding(foldl, foldr)
import Data.Aeson as J
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L hiding(foldl, foldr)


{-- -
	Creating different files for different types is probably is the right way. We can't impose structure
	in a single file haskell app?!
--}
data UserJoined  = UserJoined {userNickName ::  T.Text};


parseUserJoined v = UserJoined <$> 
                    v .: "userNickName"


genUserJoined (UserJoined v ) = object [
                        "userNickName" .= v
                        , "commandType" .= ("UserJoined" :: T.Text) ]


instance ToJSON UserJoined where
	toJSON = genUserJoined

instance FromJSON UserJoined where
    parseJSON (Object v) = parseUserJoined v
    parseJSON _          = Appl.empty


{-
-- What are the steps in our processing:
. Parse json values
. Decode to the type of the command we want.
. Process the command.
. Send response. : the application object

. 
-}


