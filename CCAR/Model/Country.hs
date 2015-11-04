module CCAR.Model.Country 
	( 
		setupCountries
	)
where

import Control.Monad.IO.Class 
import Control.Monad
import Control.Monad.Logger 
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe
import Control.Applicative as Appl
import Database.Persist
import Database.Persist.Postgresql as Postgresql 
import Database.Persist.TH 
import CCAR.Main.DBUtils
import CCAR.Command.ApplicationError 
import Data.Text as T 
import qualified CCAR.Main.EnumeratedTypes as EnumeratedTypes 
import qualified CCAR.Main.GroupCommunication as GC
import Data.Aeson
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import Data.Monoid
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L 

import GHC.Generics
import Data.Data
import Data.Typeable 
import Data.Time
import CCAR.Main.Util 
import System.Log.Logger as Logger
import CCAR.Parser.CSVParser as CSVParser
import System.IO 


data CRUD = Create | Read | C_Update | Delete
    deriving(Show, Eq, Read, Data, Generic, Typeable)



type ISO_3 = T.Text 
type ISO_2 = T.Text 
type Name = T.Text 
type Domain = T.Text 

add :: ISO_3 -> ISO_2 -> Name -> Domain -> IO (Key Country)
add a b c d = dbOps $ do 
	country <- getBy $ UniqueISO3 a
	case country of 
		Nothing -> insert $ Country c a b d 
		Just (Entity k v) -> do 
			liftIO $ Logger.debugM iModuleName 
					("Country " ++ (T.unpack c)  ++ " already exists")
			return k

iModuleName = "CCAR.Model.Country"
parseLine aLine =  
	add (T.pack $ aLine !! 2) 
		(T.pack $ aLine !! 1) 
		(T.pack $ aLine !! 3) 
		(T.pack $ aLine !! 4)
--setupCountries :: FilePath -> IO [Key Country]
parseCountries aHandle = do 
	inputLine <- hGetContents aHandle 
	parsedOutput <- return $ CSVParser.parseCSV inputLine
	x <- case parsedOutput of 
		Left e -> do 
					putStrLn "Error processing file" 
					print e
		Right r ->  mapM_ (\line -> do 
							print line
							parseLine line) r  
	return x

setupCountries aFileName = do 
	handle <- openFile aFileName ReadMode 
	parseCountries handle


