{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module UserRegistrationRecord
    ( UserRegistrationRecord (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.SafeCopy
import Data.Text (Text)
import GHC.Generics (Generic)

data UserRegistrationRecord =
    UserRegistrationRecord { loginName   :: !Text
                           , displayName :: !Text
                           , password    :: !Text }
    deriving (Generic, Show)

instance FromJSON UserRegistrationRecord
instance ToJSON UserRegistrationRecord
instance Hashable UserRegistrationRecord

$(deriveSafeCopy 0 'base ''UserRegistrationRecord)
