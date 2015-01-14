{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module UserManagementTests (suite) where

import ProgramExecutor (ProcSpec, withAstralServer)
import RESTClient (post)

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, decodeStrict)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import GHC.Generics

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

data UserRegistrationCard =
    UserRegistrationCard { loginName   :: !T.Text
                         , displayName :: !T.Text
                         , password    :: T.Text }
    deriving (Generic, Show)

data Resource = Resource { url :: !T.Text }
    deriving (Generic, Show)

instance FromJSON UserRegistrationCard
instance ToJSON UserRegistrationCard

instance FromJSON Resource
instance ToJSON Resource

suite :: Test.Framework.Test
suite =
    testGroup "User management tests"
    [ testCase "Shall accept user registration" 
               shallAcceptRegistrationTest
    , testCase "Shall reject duplicate registration" 
               shallRejectDuplicateRegistrationTest
    , testCase "Shall give different answers to different registrations"
               shallDifferentiateRegistrationsTest
    ]

shallAcceptRegistrationTest :: Assertion
shallAcceptRegistrationTest =
    withAstralServer defaultServer $ do
      let user = UserRegistrationCard { loginName   = "mr_x"
                                      , displayName = "Mister X"
                                      , password    = "mypasswd" }
      (code, cookie, payload) <- post "127.0.0.1" 8888 "/user" user

      assertResponseCode 201 code
      let maybeResource = decodeStrict payload
      assertResource maybeResource
      assertCookie cookie
      let (name, _) = splitCookie $ fromJust cookie
      assertEqual "Name should be" "session" name

      assertPrefix "/user/" $ url (fromJust maybeResource)

shallRejectDuplicateRegistrationTest :: Assertion
shallRejectDuplicateRegistrationTest =
    withAstralServer defaultServer $ do
      let user = UserRegistrationCard { loginName   = "mr_x"
                                      , displayName = "Mister X"
                                      , password    = "mypasswd" }
      void $ post "127.0.0.1" 8888 "/user" user
      (code, cookie, payload) <- post "127.0.0.1" 8888 "/user" user

      assertResponseCode 409 code
      assertNoCookie cookie
      assertEqual "Message should be" "User is already registered" payload

shallDifferentiateRegistrationsTest :: Assertion
shallDifferentiateRegistrationsTest =
    withAstralServer defaultServer $ do
      let user1 = UserRegistrationCard { loginName   = "mr_x"
                                       , displayName = "Mister X"
                                       , password    = "mypasswd" }
          user2 = UserRegistrationCard { loginName   = "mr_y"
                                       , displayName = "Mister X"
                                       , password    = "mypasswd" }
      (code1, cookie1, payload1) <- post "127.0.0.1" 8888 "/user" user1
      (code2, cookie2, payload2) <- post "127.0.0.1" 8888 "/user" user2

      assertResponseCode 201 code1
      assertResponseCode 201 code2

      let maybeResource1 = decodeStrict payload1
      assertResource maybeResource1
      let maybeResource2 = decodeStrict payload2
      assertResource maybeResource2

      assertCookie cookie1
      assertCookie cookie2

      let (name1, value1) = splitCookie $ fromJust cookie1
          (name2, value2) = splitCookie $ fromJust cookie2

      assertEqual "Equal cookie names" name1 name2
      assertBool "Unequal cookie values" $ value1 /= value2

      let (prefix1, suffix1) = splitUrl $ url (fromJust maybeResource1)
          (prefix2, suffix2) = splitUrl $ url (fromJust maybeResource2)
      assertEqual "Equal url prefixes" prefix1 prefix2
      assertBool "Unequal uel suffixes" $ suffix1 /= suffix2

defaultServer :: ProcSpec
defaultServer = ("./dist/build/astral-mining-server/astral-mining-server"
                , ["--port=8888"])

assertResponseCode :: Int -> Int -> Assertion
assertResponseCode expected actual = 
    assertEqual "ResponseCode shall be equal" expected actual

assertResource :: Maybe Resource -> Assertion
assertResource (Just _) = return ()
assertResource _        = assertBool "Shall be a Resource" False

assertPrefix :: T.Text -> T.Text -> Assertion
assertPrefix expectedPrefix theUrl =
    let (prefix, _) = splitUrl theUrl
    in assertEqual "Prefix shall be equal" expectedPrefix prefix

assertCookie :: Maybe BS.ByteString -> Assertion
assertCookie Nothing = assertBool "Shall be a cookie" False
assertCookie _       = return ()

assertNoCookie :: Maybe BS.ByteString -> Assertion
assertNoCookie (Just _) = assertBool "Shall be no cookie" False
assertNoCookie _        = return ()

splitUrl :: T.Text -> (T.Text, T.Text)
splitUrl theUrl =
    let len     = T.length theUrl
        theUrl' = T.reverse theUrl
        lastSep = maybe len id $ T.findIndex ('/' ==) theUrl'
    in T.splitAt (len - lastSep) theUrl

splitCookie :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitCookie cookie =
    let len   = BS.length cookie
        index = maybe len id $ BS.findIndex ('=' ==) cookie
    in BS.splitAt index cookie
    
