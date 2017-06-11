-- | Collection Specs
module UtilsSpec.CollectionSpec where

import Data.Char
import Data.Hashable
import Test.Hspec

import Cyrats

import Shortcuts

someCollection :: Collection Char
someCollection = fromList ['a' .. 'z']

someEmptyCollection :: Collection ()
someEmptyCollection = empty

collectionSpec :: Spec
collectionSpec =
    describe "Collection" $ do
        describe "killAt" $ do
            it "fails on empty collection" $
                shouldExplode $ killAt (hash ()) someEmptyCollection
            it "fails if key not found" $
                let badKey = hash 'A'
                in shouldExplode $ killAt badKey someCollection
            it "works for the valid key" $
                let ((k, v):xs) = toList someCollection
                    c = fromList (map snd xs)
                in killAt k someCollection `shouldGet` (v, c)
        describe "modifyAt" $ do
            it "fails on empty collection" $
                shouldExplode $ modifyAt (hash ()) pure someEmptyCollection
            it "fails if key not found" $
                let badKey = hash 'A'
                in shouldExplode $ modifyAt badKey pure someCollection
            it "works for the valid keys" $
                let changedValues = do
                        let coll = fromList "abc"
                            goUpper k = modifyAt (hash k) (pure . toUpper)
                        coll' <- goUpper 'a' coll
                        coll'' <- goUpper 'c' coll'
                        return . map snd . toList $ coll''
                in changedValues `shouldGet` "AbC"
