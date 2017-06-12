-- | Garage Specs
module DomainSpec.GarageSpec
    ( garageSpec
    ) where

import Control.Lens hiding (like)
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
            garage [] [] ^. gModules `shouldSatisfy` likeList []
            garage [] [] ^. gHulls `shouldSatisfy` likeList []
        describe "addHull,addModule" $ it "just work" $
            let [m1, m2] = catMaybes [m 1 2 3, m 3 2 1]
                h = hull (m 2 2 2) (m 3 3 3) Nothing
                g = garage [] [] & addModule m1 & addModule m2 & addHull h
            in do g ^. gModules `shouldSatisfy` like (fromList [m1, m2])
                  g ^. gHulls `shouldSatisfy` like (fromList [h])
        describe "mountModule" $ it "just works" $
            let ms = catMaybes [m 0 0 1, m 0 2 0, m 0 0 3, m 1 1 1, m 2 2 2]
                hs =
                    [ hull Nothing Nothing (m 3 3 3)
                    , hull Nothing Nothing Nothing
                    ]
                g = garage ms hs
                [mk1, mk2, mk3, mk4] = g ^. gModules . to toList . to (map fst)
                [hk1, hk2] = g ^. gHulls . to toList . to (map fst)
                g' =
                    fromRight $ mountModule mk1 hk2 HullHead g >>=
                    mountModule mk2 hk2 HullBody >>=
                    mountModule mk3 hk2 HullTail >>=
                    mountModule mk4 hk1 HullBody
            in do g' ^. gModules `shouldSatisfy` likeList [fromJust $ m 2 2 2]
                  g' ^. gHulls `shouldSatisfy`
                      likeList
                          [ hull Nothing (m 1 1 1) (m 3 3 3)
                          , hull (m 0 0 1) (m 0 2 0) (m 0 0 3)
                          ]
  where
    likeList
        :: Ord a
        => [a] -> Collection a -> Bool
    likeList = like . fromList
