{-# LANGUAGE OverloadedStrings #-}

-- | Garage Specs
module DomainSpec.GarageSpec
    ( garageSpec
    ) where

import Control.Lens
import Data.Maybe
import Test.Hspec

import Cyrats

import Shortcuts

emptyGarage :: Garage
emptyGarage = garage [] []

garageSpec :: Spec
garageSpec =
    describe "Garage" $ do
        describe "garage" $ it "just works" $ do
            garage [] [] ^. gModules `shouldBeLike` mods []
            garage [] [] ^. gHulls `shouldBeLike` hulls []
        addingSpec
        mountModuleSpec

addingSpec :: Spec
addingSpec =
    describe "addHull,addModule" $ it "just work" $
    let g =
            garage [] [] & addModule "123" & addModule "321" &
            addHull "222 333 _"
    in do g ^. gModules `shouldBeLike` mods ["123", "321"]
          g ^. gHulls `shouldBeLike` hulls ["222 333 _"]

mountModuleSpec :: Spec
mountModuleSpec =
    describe "mountModule" $ it "just works" $ do
        let ms = ["001", "020", "003", "111", "222"]
            hs = ["_ _ 333", "_ _ _"]
            g = garage ms hs
            [mk1, mk2, mk3, mk4, mk5] = g ^. gModules . keys
            [hk1, hk2] = g ^. gHulls . keys
            g' :: Garage
            g' =
                fromRight $ pure g >>=
                bindAll
                    [ mountModule mk1 hk2 HullHead
                    , mountModule mk2 hk2 HullBody
                    , mountModule mk3 hk2 HullTail
                    , mountModule mk4 hk1 HullBody
                    ]
        g' ^. gModules `shouldBeLike` mods ["222"]
        g' ^. gHulls `shouldBeLike` hulls ["_ 111 333", "001 020 003"]

infixr 1 `shouldBeLike`

shouldBeLike
    :: (Ord a, Show a)
    => Collection k a -> Collection k a -> Expectation
shouldBeLike x = shouldSatisfy x . looksLike

mods :: [RatModule] -> Collection ModuleKey RatModule
mods = fromListOf _ModuleKey

hulls :: [RatHull] -> Collection HullKey RatHull
hulls = fromListOf _HullKey
