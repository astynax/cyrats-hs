module Main where

import Control.Lens
import Control.Monad.Except
import Data.Maybe
import Data.Monoid
import Test.Hspec

import Cyrats

import DomainSpec
import Shortcuts
import UtilsSpec

main :: IO ()
main =
    hspec $ do
        shortcutsSpec
        describe "Cyrats" $ do
            domainSpec
            utilsSpec
