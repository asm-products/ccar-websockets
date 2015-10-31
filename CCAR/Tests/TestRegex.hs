{-# LINE 1 "CCAR/Tests/TestRegex.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "CCAR/Tests/TestRegex.hsc" #-}
module CCAR.Tests.TestRegex 
where 

import Foreign
import Foreign.C.Types


{-# LINE 9 "CCAR/Tests/TestRegex.hsc" #-}

newtype PCREOption = PCREOption {unPCREOption :: CInt} 
		deriving (Show, Eq)


caseless  :: PCREOption
caseless  = PCREOption 1
dollar_endonly   :: PCREOption
dollar_endonly   = PCREOption 32
dotall  :: PCREOption
dotall  = PCREOption 4


combineOptions :: [PCREOption] -> [PCREOption]
combineOptions = PCREOpton . foldr ( (.|.) . unPCREOption) 0 
