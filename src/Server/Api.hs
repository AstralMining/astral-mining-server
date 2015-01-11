{-# LANGUAGE OverloadedStrings #-}
module Server.Api (router) where

import Control.Applicative ((<|>))
import Persistance.AstralState ( AcidState
                               , AstralState )
import Snap.Core ( Snap
                 , Method (..)
                 , emptyResponse
                 , method
                 , putResponse
                 , route
                 , setHeader
                 , setResponseCode
                 , writeBS )

router :: AcidState AstralState -> Snap ()
router astralState = route
         [ ("/register", method POST $ registerNewUser astralState) ]
         <|> handlerNotFound

registerNewUser :: AcidState AstralState -> Snap ()
registerNewUser astralState = return ()

handlerNotFound :: Snap ()
handlerNotFound = do
  let response = setResponseCode 404 $
                 setHeader "Content-Type" "plain/text" $
                 emptyResponse
  putResponse response
  writeBS "Resource not found"
