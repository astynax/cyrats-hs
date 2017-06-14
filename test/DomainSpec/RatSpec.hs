{-# LANGUAGE OverloadedStrings #-}

-- | Rat Specs
module DomainSpec.RatSpec
    ( ratSpec
    ) where

import Control.Lens
import Control.Monad.Except
import Data.Maybe
import Test.Hspec

import Cyrats

import Shortcuts

ratSpec :: Spec
ratSpec =
    describe "Rat" $ do
        animateSpec
        ratHullSpec

animateSpec :: Spec
animateSpec =
    describe "animate" $ do
        it "makes no rats if the hull is incomplete" $
            isNothing (animate "123 _ 456")
        it "makes a normal rat otherwise" $
            let r = animate "123 999 456"
            in [ r ^? _Just . rHealth
               , r ^? _Just . rAttack
               , r ^? _Just . rDefence
               ] ==
               [Just 14, Just 16, Just 18]

ratHullSpec :: Spec
ratHullSpec = do
    describe "isEnoughFor" $ do
        let someHull = "111 121 111"
        it "returns True if energy is enough" $ 10 `isEnoughFor` someHull
        it "returns False otherwise" $ not $ 9 `isEnoughFor` someHull
    describe "placeTo" $ do
        let m = "111" :: RatModule
        it "works if section IS empty" $ do
            placeTo HullHead m "_ _ _" `shouldGet` "111 _ _"
            placeTo HullBody m "_ _ _" `shouldGet` "_ 111 _"
            placeTo HullTail m "_ _ _" `shouldGet` "_ _ 111"
        it "fails if section ISN'T empty" $ do
            shouldExplode $ placeTo HullHead m "111 _ _"
            shouldExplode $ placeTo HullBody m "_ 111 _"
            shouldExplode $ placeTo HullTail m "_ _ 111"
    describe "removeFrom" $ do
        let [m1, m2, m3] = ["111", "222", "333"] :: [RatModule]
        it "works if section ISN'T empty" $ do
            removeFrom HullHead "111 222 333" `shouldGet` (m1, "_ 222 333")
            removeFrom HullBody "111 222 333" `shouldGet` (m2, "111 _ 333")
            removeFrom HullTail "111 222 333" `shouldGet` (m3, "111 222 _")
        it "fails if section IS empty" $ do
            let eh = "_ _ _" :: RatHull
            shouldExplode $ removeFrom HullHead eh
            shouldExplode $ removeFrom HullBody eh
            shouldExplode $ removeFrom HullTail eh
