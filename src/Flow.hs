{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Flow 
    ( doRegisterNewUser
    ) where

import AstralState ( AcidState
                   , AstralState
                   , RegisterNewUser (..)
                   , update )
import Control.Applicative ((<$>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (encode)
import Data.Hashable (Hashable, hash)
import qualified Data.Text as T
import FlowResponseAction  ( FlowResponseAction (..)
                           , ResponseCode (..) )
import GHC.Generics (Generic)
import System.Random (randomRIO)
import UserRegistrationRecord (UserRegistrationRecord)

data Resource =
    Resource { url :: !T.Text }
    deriving (Generic, Show)

instance FromJSON Resource
instance ToJSON Resource

doRegisterNewUser :: AcidState AstralState 
                  -> UserRegistrationRecord 
                  -> IO FlowResponseAction
doRegisterNewUser astralState user = do
  let hashValue = transformedHash user
  sessionValue <- mkSessionValue
  result <- update astralState $ RegisterNewUser user hashValue sessionValue
  case result of
    Right ()        ->
        return WithJSON { responseCode = C201
                        , newSession = Just sessionValue
                        , payload = Resource { url = "/user/" ~: hashValue } }
    Left errorCause ->
        return WithError { responseCode = C409
                         , cause = errorCause }
        
transformedHash :: (Hashable a) => a -> Integer
transformedHash a = toInteger $ hash a + minBound

mkSessionValue :: IO BS.ByteString
mkSessionValue = do
  numChars <- randomRIO (23, 31)
  encode <$> go numChars BS.empty
    where
      go :: Int -> BS.ByteString -> IO BS.ByteString
      go 0 xs = return xs
      go n xs = do
          char <- randomRIO (minBound, maxBound)
          go (n - 1) $ BS.cons char xs

(~:) :: T.Text -> Integer -> T.Text
base ~: num = 
    let textNum = (T.pack . show) num
    in base `T.append` textNum
