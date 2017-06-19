{-# LANGUAGE OverloadedStrings #-}

-- | Garage Specs
module DomainSpec.GarageSpec
    ( garageSpec
    ) where

import Control.Lens
import Data.Maybe
import Test.Hspec

import Cyrats

import Instances
import TestUtils

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
        unmountModuleSpec

addingSpec :: Spec
addingSpec =
    describe "addHull,addModule" $ it "just work" $
    let g =
            garage [] [] & addModule "123" & addModule "321" &
            addHull "222|333|_"
    in do g ^. gModules `shouldBeLike` mods ["123", "321"]
          g ^. gHulls `shouldBeLike` hulls ["222|333|_"]

mountModuleSpec :: Spec
mountModuleSpec =
    describe "mountModule" $ it "just works" $ do
        let ms = ["001", "020", "003", "111", "222"]
            hs = ["_|_|333", "_|_|_"]
            (g, [mk1, mk2, mk3, mk4, mk5], [hk1, hk2]) = garage' ms hs
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
        g' ^. gHulls `shouldBeLike` hulls ["_|111|333", "001|020|003"]

unmountModuleSpec :: Spec
unmountModuleSpec =
    describe "unmountModule" $ do
        it "works for minimal case" $ do
            let (g, _, [hk]) = garage' ["111"] ["_|222|_"]
                g' = fromRight . unmountModule hk HullBody $ g
            g' ^. gModules `shouldBeLike` mods ["111", "222"]
            g' ^. gHulls `shouldBeLike` hulls ["_|_|_"]
        it "works for more complicated case" $ do
            let (g, _, [hk1, hk2]) =
                    garage' ["222"] ["_|111|333", "001|020|003"]
                g' :: Garage
                g' =
                    fromRight $ pure g >>=
                    bindAll
                        [ unmountModule hk2 HullHead
                        , unmountModule hk2 HullBody
                        , unmountModule hk2 HullTail
                        , unmountModule hk1 HullBody
                        ]
            g' ^. gModules `shouldBeLike`
                mods ["222", "001", "020", "003", "111"]
            g' ^. gHulls `shouldBeLike` hulls ["_|_|333", "_|_|_"]

mods :: [RatModule] -> Collection ModuleKey RatModule
mods = fromListOf _ModuleKey

hulls :: [RatHull] -> Collection HullKey RatHull
hulls = fromListOf _HullKey

garage' :: [RatModule] -> [RatHull] -> (Garage, [ModuleKey], [HullKey])
garage' ms hs =
    (g, g ^. gModules . to (keysFor ms), g ^. gHulls . to (keysFor hs))
  where
    g = garage ms hs
    keysFor xs c =
        let ys = toList c
        in [k | v <- xs, (k, v') <- ys, v == v']
