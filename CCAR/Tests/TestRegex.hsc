{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module CCAR.Tests.TestRegex 
where 

import Foreign
import Foreign.C.Types

#include <pcre.h>

newtype PCREOption = PCREOption {unPCREOption :: CInt} 
		deriving (Show, Eq)


#{enum PCREOption, PCREOption
	, caseless = PCRE_CASELESS
	, dollar_endonly  = PCRE_DOLLAR_ENDONLY
	, dotall = PCRE_DOTALL
}


