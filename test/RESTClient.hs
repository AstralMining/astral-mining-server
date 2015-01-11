{-# LANGUAGE OverloadedStrings, TupleSections #-}
module RESTClient (get, post) where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import Network.Http.Client ( Hostname
                           , Port
                           , Method (..)
                           , Response
                           , buildRequest
                           , concatHandler
                           , emptyBody
                           , getStatusCode
                           , http
                           , openConnection
                           , receiveResponse
                           , sendRequest
                           , setAccept
                           , setContentType
                           , withConnection )
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

type Url = BS.ByteString
type ServiceReply = (Int, BS.ByteString)

get :: Hostname -> Port -> Url -> IO ServiceReply
get host port url =
    withConnection (openConnection host port) $ \connection -> do
      request <- buildRequest $ do
                   http GET url
                   setAccept "application/json"
      sendRequest connection request emptyBody
      receiveResponse connection replyHandler

post :: ToJSON a => Hostname -> Port -> Url -> a -> IO ServiceReply
post host port url payload =
    withConnection (openConnection host port) $ \connection -> do
      request <- buildRequest $ do
                   http POST url
                   setAccept "application/json"
                   setContentType "application/json"
      sendRequest connection request $ jsonBody payload
      receiveResponse connection replyHandler

jsonBody :: ToJSON a => a -> OutputStream Builder -> IO ()
jsonBody payload = Streams.write (Just (fromLazyByteString $ encode payload))

replyHandler :: Response -> InputStream BS.ByteString -> IO ServiceReply
replyHandler resp i = (getStatusCode resp,) <$> concatHandler resp i
