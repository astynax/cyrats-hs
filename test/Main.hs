module Main
    ( main
    ) where

import Control.Lens
import Control.Monad.Except
import Data.Maybe
import Data.Monoid
import Test.Hspec

import Cyrats

import DomainSpec
import TestUtils
import UtilsSpec

main :: IO ()
main =
    hspec $ do
        testUtilsSpec
        describe "Cyrats" $ do
            domainSpec
            utilsSpec
