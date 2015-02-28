{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CCAR.Command where

import Control.Monad.Trans
import Control.Monad.Reader

class Response a where 
	result :: a -> IO a 
class Command a where 
	process :: (Response b) => a -> IO b

data SendCommand = SendCommand Int deriving(Show, Eq, Read)
data ResponseSendCommand = ResponseSendCommand Int 
instance Command SendCommand where 
	process (SendCommand x) = result $ SendCommand x

instance Response SendCommand where
	result (SendCommand x) = return (SendCommand x)

{-data AnotherSendCommand = AnotherSendCommand String deriving (Show, Eq, Read)

instance Command AnotherSendCommand where
	process (AnotherSendCommand x) = return (AnotherSendCommand "Help")
-}
