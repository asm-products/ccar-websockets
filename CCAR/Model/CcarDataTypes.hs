module CCAR.Model.CcarDataTypes where
import Import
import Data.Text as Text
import CCAR.Model.Maturity

type Name = String
type ISO_3 = String
type Price = Integer
type ErrorMessage = String

type MaturityCurve = [Mat]

data Currency = Currency {iso_3 :: ISO_3} deriving (Show, Read, Eq, Generic, Typeable)
data Expiration = Exp Month Year deriving(Show, Read, Eq, Generic, Typeable)
data Strike = Str Price deriving (Show, Read, Eq, Generic, Typeable)
type Underlying = String
data Country = Country ISO_3 deriving(Show, Read, Eq, Generic, Typeable)
type CountryGroup = [Country]
type CurrencyGroup = [Currency]

type DateFormat = String

data CCARError = CCARError {msg :: Text} deriving (Read, Eq, Ord, Generic, Typeable)

instance Show CCARError where
    show a = Text.unpack $ msg a


-- The TenorValue is the value for the product that is being modeled. 
-- 
type FormatString = String

type TenorValue = Rational 

data Product = Equity Underlying | CCAROption Underlying Expiration Strike| Commodity Name 
    | CurrencyPair Currency Currency | Tenor Currency [(Mat, TenorValue)]
    | TenorOptions ISO_3 Expiration Mat        
    deriving(Show, Read, Eq, Generic, Typeable)


type Abbreviation = String

data Stress = EquityStress Product StressValue  | OptionStress Product StressValue 
                | StressError CCARError
                | EquityByGeography CountryGroup Country StressValue
                | CurrencyVegaStress Product [(Mat, StressValue)]
                 -- BasisRisk is expressed in months such as 1m, 3m 
                 -- 12 m etc, though any unit is valid
                | BasisRiskRates Currency [(Mat, Mat, StressValue)]
                | DirectionalRates Currency [ (Mat, Mat, StressValue)]
                 -- currencies such as EUR..
                | DirectionalRatesCommonCurrency [(Country, CountryGroup, Mat, StressValue)]
                | RatesVega Currency [(Expiration, Mat, StressValue)]
                | CurrencyStress Product StressValue
                | TenorStress Currency [(Mat, StressValue)]
                | TenorVegaStress Currency Mat [(Mat, StressValue)]
                | EffectiveDate String
                | Formattingg FormatString FormatType
                | FirmDetails Abbreviation String
            deriving (Eq, Show, Read, Generic, Typeable)

data FormatType = DateFormat | CurrencyFormat | DateTimeFormat deriving (Eq, Show, Read, Generic, Typeable)
data Sign = Positive | Negative deriving (Eq, Show, Read, Generic, Typeable)
data StressValue = Percentage Sign Rational | BasisPoints Sign Integer | StressValueError ErrorMessage
    deriving (Show, Read, Eq, Generic, Typeable)

instance ToJSON StressValue
instance FromJSON StressValue
instance ToJSON Stress
instance FromJSON Stress
instance ToJSON Product
instance FromJSON Product
instance ToJSON Currency
instance FromJSON Currency
instance ToJSON Country
instance FromJSON Country
instance ToJSON Strike
instance FromJSON Strike
instance ToJSON Expiration
instance FromJSON Expiration
instance ToJSON CCARError
instance FromJSON CCARError
instance ToJSON FormatType
instance FromJSON FormatType
instance ToJSON Sign
instance FromJSON Sign