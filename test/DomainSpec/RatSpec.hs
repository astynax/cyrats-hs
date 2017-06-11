-- | Rat Specs
module DomainSpec.RatSpec where

import Control.Lens
import Control.Monad.Except
import Data.Maybe
import Test.Hspec

import Cyrats

import Shortcuts

ratSpec :: Spec
ratSpec = describe "Rat" $ do ratHullSpec

ratHullSpec = do
    describe "animate" $ do
        it "makes no rats if the hull is incomplete" $
            animate (hull (m 1 2 3) Nothing (m 4 5 6)) `shouldSatisfy`
            isNothing
        it "makes a normal rat otherwise" $
            animate (hull (m 1 2 3) (m 10 20 30) (m 4 5 6)) `shouldSatisfy`
            (\r ->
                 and
                     [ r ^? _Just . rHealth == Just 15
                     , r ^? _Just . rAttack == Just 27
                     , r ^? _Just . rDefence == Just 39
                     ])
    describe "isEnoughFor" $ do
        let someHull = hull (m 1 1 1) (m 1 2 1) (m 1 1 1)
        it "returns True if energy is enough" $ someHull `shouldSatisfy`
            isEnoughFor 10
        it "returns False otherwise" $ someHull `shouldSatisfy`
            (not . isEnoughFor 9)
    describe "placeTo" $ do
        let eh = hull Nothing Nothing Nothing
            fh = hull mm mm mm
            mm@(Just m') = m 1 1 1
        it "works if section IS empty" $ do
            placeTo HullHead m' eh `shouldGet` hull mm Nothing Nothing
            placeTo HullBody m' eh `shouldGet` hull Nothing mm Nothing
            placeTo HullTail m' eh `shouldGet` hull Nothing Nothing mm
        it "fails if section ISN'T empty" $ do
            shouldExplode $ placeTo HullHead m' fh
            shouldExplode $ placeTo HullBody m' fh
            shouldExplode $ placeTo HullTail m' fh
    describe "removeFrom" $ do
        let eh = hull Nothing Nothing Nothing :: RatHull
            fh = hull m1 m2 m3
            (m1, m2, m3) = (m 1 1 1, m 2 2 2, m 3 3 3)
        it "works if section ISN'T empty" $ do
            removeFrom HullHead fh `shouldGet` (fromJust m1, hull Nothing m2 m3)
            removeFrom HullBody fh `shouldGet` (fromJust m2, hull m1 Nothing m3)
            removeFrom HullTail fh `shouldGet` (fromJust m3, hull m1 m2 Nothing)
        it "fails if section IS empty" $ do
            shouldExplode $ removeFrom HullHead eh
            shouldExplode $ removeFrom HullBody eh
            shouldExplode $ removeFrom HullTail eh
