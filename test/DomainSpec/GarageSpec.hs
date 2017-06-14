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
    let [m1, m2] = catMaybes [m 1 2 3, m 3 2 1]
        h = hull (m 2 2 2) (m 3 3 3) Nothing
        g = garage [] [] & addModule m1 & addModule m2 & addHull h
    in do g ^. gModules `shouldBeLike` mods [m1, m2]
          g ^. gHulls `shouldBeLike` hulls [h]

mountModuleSpec :: Spec
mountModuleSpec =
    describe "mountModule" $ it "just works" $ do
        let ms = catMaybes [m 0 0 1, m 0 2 0, m 0 0 3, m 1 1 1, m 2 2 2]
            hs = [hull Nothing Nothing (m 3 3 3), hull Nothing Nothing Nothing]
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
        g' ^. gModules `shouldBeLike` mods [fromJust $ m 2 2 2]
        g' ^. gHulls `shouldBeLike`
            hulls
                [ hull Nothing (m 1 1 1) (m 3 3 3)
                , hull (m 0 0 1) (m 0 2 0) (m 0 0 3)
                ]

infixr 1 `shouldBeLike`

shouldBeLike
    :: (Ord a, Show a)
    => Collection k a -> Collection k a -> Expectation
shouldBeLike x = shouldSatisfy x . looksLike

mods :: [RatModule] -> Collection ModuleKey RatModule
mods = fromListOf _ModuleKey

hulls :: [RatHull] -> Collection HullKey RatHull
hulls = fromListOf _HullKey
