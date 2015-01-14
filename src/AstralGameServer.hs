module Main (main) where

import Api (router)
import AstralState (initialState, openLocalState)
import OptCracker (crack, usage)
import Snap.Http.Server ( defaultConfig
                        , httpServe
                        , setPort )
import System.Environment (getArgs)

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
