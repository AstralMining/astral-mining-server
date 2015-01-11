module Util.OptCracker
    ( OptionSet
    , crack
    , usage
    ) where

import Data.List (find)
import System.Console.GetOpt 
    ( OptDescr(..)
    , ArgDescr (..)
    , ArgOrder (..)
    , getOpt
    , usageInfo )

type OptionSet = (Bool, Maybe Int)

data CommandOption = Help | Port !String
    deriving (Eq, Show)

crack :: [String] -> Either String OptionSet
crack args =
    case getOpt Permute options args of
      ([], [], [])     -> Right (True, Nothing)
      (opts, [], [])   -> Right $ determineOptions opts
      (_, nonOpts, []) -> Left $ unwords nonOpts
      (_, _, errors)   -> Left $ unwords errors

usage :: String
usage = usageInfo "Usage: astral-mining-server [OPTIONS ...]\n" options

determineOptions :: [CommandOption] -> OptionSet
determineOptions opts =
    let help = Help `elem` opts
        port = maybePort opts
    in (help, port)

maybePort :: [CommandOption] -> Maybe Int
maybePort opts = do
  Port portString <- port `find` opts
  maybeRead portString
    where
      port (Port _) = True
      port _        = False

maybeRead :: Read a => String -> Maybe a
maybeRead x =
    case reads x of
      [(val, "")] -> Just val
      _           -> Nothing

options :: [OptDescr CommandOption]
options =
    [ Option ['h', '?'] ["help"] (NoArg Help) "This help screen" 
    , Option ['p']      ["port"] (ReqArg Port "NUM") "Server port number"
    ]
