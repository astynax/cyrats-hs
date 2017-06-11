-- | Garage Specs
module DomainSpec.GarageSpec where

import Control.Lens
import Data.Hashable
import Data.Maybe
import Test.Hspec

import Cyrats

import Shortcuts

emptyGarage :: Garage
emptyGarage = garage [] []

garageSpec :: Spec
garageSpec =
    describe "Garage" $ do
        describe "garage" $
            it "just works" $ do
                garage [] [] ^. modules `shouldBe` []
                garage [] [] ^. hulls `shouldBe` []
        describe "addHull,addModule" $
            it "just work" $ do
                let (m1:m2:[]) = catMaybes [m 1 2 3, m 3 2 1]
                    h = hull (m 2 2 2) (m 3 3 3) Nothing
                    g = garage [] [] & addModule m1 & addModule m2 & addHull h
                g ^. modules `shouldBe` [(hash m1, m1), (hash m2, m2)]
                g ^. hulls `shouldBe` [(hash h, h)]
        describe "mountModule" $
            it "just works" $
            let ms = catMaybes [m 0 0 1, m 0 2 0, m 0 0 3, m 1 1 1, m 2 2 2]
                hs =
                    [ hull Nothing Nothing (m 3 3 3)
                    , hull Nothing Nothing Nothing
                    ]
                g = garage ms hs
                ((mk1, m1):(mk2, m2):(mk3, m3):(mk4, m4):(mk5, m5):[]) =
                    g ^. modules
                ((hk1, h1):(hk2, h2):[]) = g ^. hulls
                res =
                    mountModule mk1 hk2 HullHead g >>=
                    mountModule mk2 hk2 HullBody >>=
                    mountModule mk3 hk2 HullTail >>=
                    mountModule mk4 hk1 HullBody
            in pendingWith "Collection should be rewritten with autoseq keys"
{-            in res `shouldGet`
               garage
                   [m5]
                   [ hull Nothing (m 1 1 1) (m 3 3 3)
                   , hull (m 0 0 1) (m 0 2 0) (m 0 0 3)
                   ]
-}
