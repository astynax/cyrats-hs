-- | Collection Specs
module UtilsSpec.CollectionSpec
    ( collectionSpec
    ) where

import Data.Char
import Test.Hspec

import Cyrats

import Shortcuts

someCollection :: Collection Char
someCollection = fromList ['a' .. 'z']

someEmptyCollection :: Collection ()
someEmptyCollection = empty

someBadKey :: Key
someBadKey = -1

collectionSpec :: Spec
collectionSpec =
    describe "Collection" $ do
        describe "killAt" $ do
            it "fails on empty collection" $
                shouldExplode $ killAt 0 someEmptyCollection
            it "fails if key not found" $
                shouldExplode $ killAt someBadKey someCollection
            it "works for the valid key" $
                let ((k, v1):kvs) = toList someCollection
                    (v2, res) = fromRight $ killAt k someCollection
                in res `like` fromList (map snd kvs)
        describe "modifyAt" $ do
            it "fails on empty collection" $
                shouldExplode $ modifyAt 0 pure someEmptyCollection
            it "fails if key not found" $
                shouldExplode $ modifyAt someBadKey pure someCollection
            it "works for the valid keys" $
                let changedValues = do
                        let coll = fromList "abc"
                            [k1, k2] =
                                [ k
                                | (k, v) <- toList coll
                                , v == 'a' || v == 'c'
                                ]
                            goUpper k = modifyAt k (pure . toUpper)
                        coll' <- goUpper k1 coll
                        coll'' <- goUpper k2 coll'
                        return . map snd . toList $ coll''
                in changedValues `shouldGet` "AbC"
