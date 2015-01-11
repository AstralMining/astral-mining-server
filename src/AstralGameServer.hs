module Main (main) where

import Persistance.AstralState (initialState, openLocalState)
import Snap.Http.Server ( defaultConfig
                        , httpServe
                        , setPort )
import System.Environment (getArgs)

import Server.Api (router)
import Util.OptCracker (crack, usage)

main :: IO ()
main = do
  args <- getArgs
  case crack args of
    Left errorDescription -> do putStr errorDescription
                                putStr usage
    Right (True, _)        -> putStr usage
    Right (_, Nothing)     -> putStr usage
    Right (_, Just port)   -> startServer port

startServer :: Int -> IO ()
startServer port = do
  let config = setPort port defaultConfig
  astralState <- openLocalState initialState
  httpServe config $ router astralState
