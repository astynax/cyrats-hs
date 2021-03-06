-- | Collection Specs
module UtilsSpec.CollectionSpec
    ( collectionSpec
    ) where

import Control.Lens
import Data.Char
import Test.Hspec

import Cyrats

import TestUtils

_Identity :: Iso' (Identity a) a
_Identity = iso runIdentity Identity

someCollection :: Collection (Identity Int) Char
someCollection = fromListOf _Identity ['a' .. 'z']

someEmptyCollection :: Collection (Identity Int) Char
someEmptyCollection = fromListOf _Identity []

someBadKey :: Identity Int
someBadKey = _Identity # (-1)

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
                in res `looksLike` fromListOf _Identity (map snd kvs)
        describe "modifyAt'" $ do
            it "fails on empty collection" $
                shouldExplode $ modifyAt' 0 pure someEmptyCollection
            it "fails if key not found" $
                shouldExplode $ modifyAt' someBadKey pure someCollection
            it "works for the valid keys" $
                let changedValues =
                        fromRight $ do
                            let coll = fromListOf _Identity "abc"
                                [k1, k2] =
                                    [ k
                                    | (k, v) <- toList coll
                                    , v == 'a' || v == 'c'
                                    ]
                                goUpper k = modifyAt' k (pure . toUpper)
                            coll' <- goUpper k1 coll
                            goUpper k2 coll'
                in changedValues `shouldBeLike` fromListOf _Identity "AbC"
