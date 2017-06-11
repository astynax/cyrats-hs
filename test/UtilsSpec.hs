-- | Utils Specs
module UtilsSpec where

import Test.Hspec

import UtilsSpec.CollectionSpec

utilsSpec :: Spec
utilsSpec = describe "Utils" $ collectionSpec
