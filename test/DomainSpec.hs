-- | Domain Specs
module DomainSpec
    ( domainSpec
    ) where

import Test.Hspec

import DomainSpec.GarageSpec
import DomainSpec.RatSpec

domainSpec :: Spec
domainSpec =
    describe "Domain" $ do
        garageSpec
        ratSpec
