module CCAR.Model.Country 
	( 
		setupCountries
		, cleanupCountries
		, startup
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
import System.Environment(getEnv)

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

remove aCountryCode	= dbOps $ deleteBy $ UniqueISO3 aCountryCode


iModuleName = "CCAR.Model.Country"

deleteLine aLine = remove (T.pack $ aLine !! 2)

insertLine aLine =  
	add (T.pack $ aLine !! 2) 
		(T.pack $ aLine !! 1) 
		(T.pack $ aLine !! 3) 
		(T.pack $ aLine !! 4)
--setupCountries :: FilePath -> IO [Key Country]
parseCountries aHandle dbFunction = do 
	inputLine <- hGetContents aHandle 
	parsedOutput <- return $ CSVParser.parseCSV inputLine
	x <- case parsedOutput of 
		Left e -> do 
					putStrLn "Error processing file" 
					print e
		Right (h:r) ->  mapM_ (\line -> do 
							print line
							dbFunction line) r  
	return x

deleteCountries aHandle = do 
	inputLine <- hGetContents aHandle 
	parseCountries aHandle deleteLine


setupCountries aFileName = do 
	handle <- openFile aFileName ReadMode 
	parseCountries handle insertLine


cleanupCountries aFileName = do 
	handle <- openFile aFileName ReadMode 
	print "Opening handle" 
	parseCountries handle deleteLine 

startup = do 
	dataDirectory <- getEnv("DATA_DIRECTORY");
	setupCountries (dataDirectory ++ "/" ++ "Country.csv")