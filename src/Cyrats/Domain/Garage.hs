{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Garage definition
module Cyrats.Domain.Garage
    ( Garage()
    , garage
      -- * getters
    , gHulls
    , gModules
      -- * operations with garages
    , addModule
    , addHull
    , mountModule
    ) where

import Control.Lens
import Control.Monad.Except

import Cyrats.Domain.Rat
import Cyrats.Utils
import qualified Cyrats.Utils as Collection

data Garage = Garage
    { _gModules :: Collection RatModule
    , _gHulls :: Collection RatHull
    } deriving (Show)

makeLenses ''Garage

garage :: [RatModule] -> [RatHull] -> Garage
garage ms hs = Garage (fromList ms) (fromList hs)

addModule :: RatModule -> Garage -> Garage
addModule = over gModules . insert

addHull :: RatHull -> Garage -> Garage
addHull = over gHulls . insert

mountModule
    :: Key
    -- ^ module key
    -> Key
    -- ^ hull key
    -> HullSection
    -> Garage
    -> Possible Garage
mountModule mk hk s g = do
    (m, ms) <- killAt mk $ g ^. gModules
    hs <- modifyAt hk (placeTo s m) $ g ^. gHulls
    pure $ g & gModules .~ ms & gHulls .~ hs
