{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Rat unit definition
module Cyrats.Domain.Rat
    ( Module(..)
    , RatModule
    , HullSection(..)
    , Hull(..)
    , RatHull
      -- * constructors
    , emptyHull
      -- * optics
    , rHealth
    , rAttack
    , rDefence
      -- * operations
    , calcStats
    , animate
    , isEnoughFor
    , placeTo
    , removeFrom
    ) where

import Control.Lens
import Control.Monad.Except
import Data.Foldable as F
import Data.Maybe
import Data.Monoid

import Cyrats.Utils

newtype Module a = Module
    { _mStats :: (a, a, a)
      -- ^ Module stats: HP,AP,DP
    } deriving (Eq, Foldable, Functor, Monoid, Ord, Show)

makeLenses ''Module

type RatModule = Module Int

data HullSection
    = HullHead
    | HullBody
    | HullTail
    deriving (Show)

data Hull a = Hull
    { _rhHead :: !(Maybe a)
    , _rhBody :: !(Maybe a)
    , _rhTail :: !(Maybe a)
    } deriving (Eq, Foldable, Functor, Ord, Show)

makeLenses ''Hull

type RatHull = Hull RatModule

data Rat = Rat
    { _rHull :: !RatHull
    , _rHealth :: !Int
    , _rAttack :: !Int
    , _rDefence :: !Int
    } deriving (Show)

makeLenses ''Rat

emptyHull :: Hull a
emptyHull = Hull Nothing Nothing Nothing

calcStats :: RatHull -> (Int, Int, Int)
calcStats = _mStats . fmap getSum . foldMap (fmap Sum)

animate :: RatHull -> Maybe Rat
animate rh
    | length rh < 3 = Nothing
    | otherwise =
        let (h, a, d) = calcStats rh
        in Just $ Rat rh h a d

isEnoughFor :: Int -> RatHull -> Bool
isEnoughFor energy = (<= energy) . sum . fmap sum

placeTo :: HullSection -> a -> Hull a -> Possible (Hull a)
placeTo s m h =
    if has _Just $ h ^. atSection s
        then throwError "Section already contains the module!"
        else pure $ h & atSection s .~ Just m

removeFrom :: HullSection -> Hull a -> Possible (a, Hull a)
removeFrom s h =
    let (mm, h') = h & atSection s <<.~ Nothing
    in case mm of
           Nothing -> throwError "Section is empty!"
           Just m -> pure (m, h')

-- helpers
atSection :: HullSection -> Lens' (Hull a) (Maybe a)
atSection HullHead = rhHead
atSection HullBody = rhBody
atSection HullTail = rhTail
