{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, RecordWildCards #-}
module AstralState 
    ( AcidState
    , AstralState
    , RegisterNewUser (..)
    , openLocalState
    , initialState
    , update
    , query
    ) where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Text (Text)
import Data.Typeable (Typeable)
import UserRegistrationRecord (UserRegistrationRecord (..))

data UserKey =
      UserName !Text
    | HashValue !Integer
    | SessionValue !BS.ByteString
      deriving (Eq, Ord, Show)

data UserRecord =
    UserRecord { loginName   :: !Text
               , displayName :: !Text
               , password    :: !Text }
    deriving (Show, Typeable)

type UserMap = Map.Map UserKey UserRecord

data AstralState = AstralState { userMap :: !UserMap }
    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''UserKey)
$(deriveSafeCopy 0 'base ''UserRecord)
$(deriveSafeCopy 0 'base ''AstralState)

lookupUser :: UserKey -> Query AstralState (Maybe UserRecord)
lookupUser key = do
  dataModel <- ask
  return $ Map.lookup key (userMap dataModel)
    
registerNewUser :: UserRegistrationRecord
                -> Integer
                -> BS.ByteString
                -> Update AstralState (Either LBS.ByteString ())
registerNewUser UserRegistrationRecord {..} hashValue sessionValue = do
  maybeRecord <- liftQuery $ lookupUser (UserName loginName)
  case maybeRecord of
    Nothing -> do
      let userRecord = UserRecord { loginName   = loginName
                                  , displayName = displayName
                                  , password    = password }
      modify $ \s -> 
          AstralState $ 
                      Map.insert (SessionValue sessionValue) userRecord $
                      Map.insert (HashValue hashValue) userRecord $
                      Map.insert (UserName loginName) userRecord (userMap s)
      
      return (Right ())

    _       -> return (Left "User is already registered")

$(makeAcidic ''AstralState ['lookupUser, 'registerNewUser])

initialState :: AstralState
initialState = AstralState { userMap = Map.empty }
