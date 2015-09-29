{--License: license.txt --}
module CCAR.Main.Util where
import Data.Text as T  hiding(foldl, foldr)
import Data.Aeson as J
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L hiding(foldl, foldr)
import System.Locale as Loc 
import Data.Time
import Network.HTTP.Client as HttpClient

serialize :: (ToJSON a) => a -> T.Text 
serialize  = L.toStrict . E.decodeUtf8 . En.encode  


parseDate (Just aDate) = parseTime Loc.defaultTimeLocale (Loc.rfc822DateFormat) (aDate)


