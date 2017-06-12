-- | Utils Specs
module UtilsSpec
    ( utilsSpec
    ) where

import Test.Hspec

import UtilsSpec.CollectionSpec

utilsSpec :: Spec
utilsSpec = describe "Utils" collectionSpec
