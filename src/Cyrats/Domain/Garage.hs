{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Garage definition
module Cyrats.Domain.Garage
    ( Garage()
    , HullKey()
    , ModuleKey()
      -- * constructors & isos
    , _HullKey
    , _ModuleKey
    , garage
      -- * getters
    , gHulls
    , gModules
      -- * operations with garages
    , addModule
    , addHull
    , mountModule
    , unmountModule
    ) where

import Control.Lens
import Control.Monad.Except

import Cyrats.Domain.Rat
import Cyrats.Utils
import qualified Cyrats.Utils as Collection

newtype ModuleKey =
    ModuleKey Int
    deriving (Show, Eq)

makePrisms ''ModuleKey

newtype HullKey =
    HullKey Int
    deriving (Show, Eq)

makePrisms ''HullKey

data Garage = Garage
    { _gModules :: Collection ModuleKey RatModule
    , _gHulls :: Collection HullKey RatHull
    } deriving (Show)

makeLenses ''Garage

garage :: [RatModule] -> [RatHull] -> Garage
garage ms hs = Garage (fromListOf _ModuleKey ms) (fromListOf _HullKey hs)

addModule :: RatModule -> Garage -> Garage
addModule = over gModules . insert

addHull :: RatHull -> Garage -> Garage
addHull = over gHulls . insert

mountModule :: ModuleKey -> HullKey -> HullSection -> Garage -> Possible Garage
mountModule mk hk s g = do
    (m, ms) <- killAt mk $ g ^. gModules
    hs <- modifyAt' hk (placeTo s m) $ g ^. gHulls
    pure $ g & gModules .~ ms & gHulls .~ hs

unmountModule :: HullKey -> HullSection -> Garage -> Possible Garage
unmountModule hk s g = do
    (m, hs) <- modifyAt hk (removeFrom s) $ g ^. gHulls
    pure $ g & gModules %~ insert m & gHulls .~ hs
