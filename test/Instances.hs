{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Instances
    ( module Instances
    ) where

import Data.Char
import Data.String
import qualified Data.Text as T

import Cyrats

instance IsString RatModule where
    fromString [x, y, z]
        | isDigit x && isDigit y && isDigit z =
            Module (digitToInt x, digitToInt y, digitToInt z)
        | otherwise = error "Malformed string!"

instance IsString RatHull where
    fromString s =
        case T.splitOn "|" $ fromString s of
            [x, y, z] -> Hull (mkMod x) (mkMod y) (mkMod z)
            _ -> error "Malformed string!"
      where
        mkMod "_" = Nothing
        mkMod x = Just . fromString $ T.unpack x
