{-# LANGUAGE ExistentialQuantification #-}
module FlowResponseAction 
    ( FlowResponseAction (..)
    , ResponseCode (..)
    , uncode
    ) where

import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

data ResponseCode = C200 | C201 | C404 | C409
    deriving Show

data FlowResponseAction = 
      forall a. ToJSON a => WithJSON 
                 { responseCode :: !ResponseCode
                 , newSession   :: !(Maybe BS.ByteString)
                 , payload      :: !a }
    | WithError { responseCode :: !ResponseCode
                , cause        :: !LBS.ByteString }

uncode :: ResponseCode -> Int
uncode C200 = 200
uncode C201 = 201
uncode C404 = 404
uncode C409 = 409
