module CCAR.Main.UserJoined 
	(UserJoined(..), userJoined, parseUserJoined
		, userLoggedIn, UserLoggedIn(..)
		, userLeft, UserLeft(..)
	 , parseUserLoggedIn
	 , userBanned
	 , parseUserBanned
	 , UserBanned(..))
where 

import Data.Text as T  hiding(foldl, foldr)
import Data.Aeson as J
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L hiding(foldl, foldr)
import CCAR.Main.Util


data UserBanned = UserBanned {unBann :: T.Text}
data UserJoined  = UserJoined {userNickName ::  T.Text};
data UserLoggedIn = UserLoggedIn {userName :: T.Text};
data UserLeft = UserLeft {leftNickName :: T.Text};



parseUserBanned v = UserBanned <$>
						v .: "userName"
genUserBanned (UserBanned v) = object ["userName" .= v 
									, "commandType" .= ("UserBanned" :: T.Text)]

parseUserLeft v = UserLeft <$> 
						v .: "userName"
genUserLeft (UserLeft v) = object ["userName" .= v
									, "commandType" .= ("UserLeft" :: T.Text)]
parseUserLoggedIn v= UserLoggedIn <$>
						v .: "userName"

parseUserJoined v = UserJoined <$> 
                    v .: "userNickName"


genUserLoggedIn (UserLoggedIn v) = object [
					"userName" .= v
					, "commandType" .= ("UserLoggedIn" :: T.Text)]

genUserJoined (UserJoined v ) = object [
                        "userNickName" .= v
                        , "commandType" .= ("UserJoined" :: T.Text) ]


instance ToJSON UserBanned where 
	toJSON = genUserBanned 

instance ToJSON UserJoined where
	toJSON = genUserJoined

instance ToJSON UserLoggedIn where
	toJSON = genUserLoggedIn

instance FromJSON UserBanned where 
	parseJSON (Object v) = parseUserBanned v 
	parseJSON _		 = Appl.empty

instance FromJSON UserJoined where
    parseJSON (Object v) = parseUserJoined v
    parseJSON _          = Appl.empty

instance FromJSON UserLoggedIn where 
	parseJSON (Object v) = parseUserLoggedIn v 
	parseJSON _ 		 = Appl.empty 

instance ToJSON UserLeft where 
	toJSON = genUserLeft

instance FromJSON UserLeft where 
	parseJSON (Object v) = parseUserLeft v 
	parseJSON _ 		 = Appl.empty 

userJoined :: T.Text -> T.Text
userJoined aText = ser $ UserJoined aText

{-- When the user has successfully logged in --}
userLoggedIn :: T.Text -> T.Text 
userLoggedIn aText = ser $ UserLoggedIn aText 

userLeft :: T.Text -> T.Text 
userLeft aText = ser $ UserLeft aText 

userBanned :: T.Text -> T.Text 
userBanned aText = ser $ UserBanned aText 
	
