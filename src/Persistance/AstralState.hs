{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Persistance.AstralState 
    ( SetDummyState
    , AcidState
    , AstralState
    , openLocalState
    , initialState
    ) where

import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import Data.Typeable (Typeable)

data DataModel =
    DataModel { dummy :: !Int }
    deriving (Show)

data AstralState = AstralState DataModel
    deriving (Typeable, Show)

$(deriveSafeCopy 0 'base ''DataModel)
$(deriveSafeCopy 0 'base ''AstralState)

setDummyState :: Int -> Update AstralState ()
setDummyState x = put $ AstralState (DataModel x)

$(makeAcidic ''AstralState ['setDummyState])

initialState :: AstralState
initialState = AstralState $ DataModel 0
