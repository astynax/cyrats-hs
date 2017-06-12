-- | Utils for testing
module Shortcuts
    ( module Shortcuts
    ) where

import Control.Monad.Except
import Data.Either
import Test.Hspec

import Cyrats

shortcutsSpec :: Spec
shortcutsSpec =
    describe "Shortcuts" $
    describe "hull" $
    it "just works" $
    map
        sum
        [ hull (Just 1) (Just 2) (Just 3)
        , hull (Just 1) Nothing Nothing
        , hull Nothing (Just 2) Nothing
        , hull Nothing Nothing (Just 3)
        ] `shouldBe`
    [6, 1, 2, 3]

m :: Int -> Int -> Int -> Maybe RatModule
m x y z = Just $ Module (x, y, z)

hull :: Maybe a -> Maybe a -> Maybe a -> Hull a
hull h b t =
    run $ put HullHead h emptyHull >>= put HullBody b >>= put HullTail t
  where
    run = either (error "Oops!") id . runExcept
    put _ Nothing old = pure old
    put s (Just m) old = placeTo s m old

fromRight :: Except e a -> a
fromRight = either (error "Oops!") id . runExcept

shouldGet
    :: (Eq a, Show a)
    => Possible a -> a -> Expectation
shouldGet x y = runExcept x `shouldBe` Right y

shouldExplode
    :: Show a
    => Possible a -> Expectation
shouldExplode = flip shouldSatisfy isLeft . runExcept
