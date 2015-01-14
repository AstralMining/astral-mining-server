{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Api (router) where

import AstralState ( AcidState
                   , AstralState )
import Control.Applicative ((<|>), (<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Flow (doRegisterNewUser)
import FlowResponseAction ( FlowResponseAction (..)
                          , ResponseCode (..)
                          , uncode )
import Snap.Core ( Snap
                 , Method (..)
                 , Cookie (..)
                 , Response
                 , addResponseCookie
                 , emptyResponse
                 , method
                 , putResponse
                 , readRequestBody
                 , route
                 , setContentType
                 , setResponseCode
                 , writeLBS )

router :: AcidState AstralState -> Snap ()
router astralState = route
         [ ("/user", method POST $ registerNewUser astralState) ]
         <|> handlerNotFound

registerNewUser :: AcidState AstralState -> Snap ()
registerNewUser astralState = do
  message <- fromJust . decode <$> readRequestBody 10000
  action <- liftIO $ doRegisterNewUser astralState message
  executeAction action

handlerNotFound :: Snap ()
handlerNotFound = executeAction WithError { responseCode = C404
                                          , cause = "Resource not found" }

executeAction :: FlowResponseAction -> Snap ()
executeAction WithJSON {..} = do
  let response = setResponseCode (uncode responseCode) $
                 setContentType "application/json" $
                 maybe emptyResponse (addSessionCookie emptyResponse) newSession
  putResponse response
  writeLBS $ encode payload
      where
        addSessionCookie :: Response -> BS.ByteString -> Response
        addSessionCookie resp value = 
            let cookie = Cookie { cookieName     = "session"
                                , cookieValue    = value
                                , cookieExpires  = Nothing
                                , cookieDomain   = Nothing
                                , cookiePath     = Nothing
                                , cookieSecure   = False
                                , cookieHttpOnly = False }
            in addResponseCookie cookie resp

executeAction WithError {..} = do
  let response = setResponseCode (uncode responseCode) $
                 setContentType "text/plain" emptyResponse
  putResponse response
  writeLBS cause
