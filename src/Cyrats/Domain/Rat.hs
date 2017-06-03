{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Rat unit definition
module Cyrats.Domain.Rat where

import Data.Foldable
import Data.Maybe
import Data.Monoid

newtype Module a = Module
    { mStats :: (a, a, a)
      -- ^ Module stats: HP,AP,DP
    } deriving (Eq, Foldable, Functor, Monoid, Show)

data Hull a = Hull
    { rhHead :: !(Maybe a)
    , rhBody :: !(Maybe a)
    , rhTail :: !(Maybe a)
    } deriving (Foldable, Functor, Show)

type RatHull = Hull (Module Int)

data Rat = Rat
    { rHull :: !RatHull
    , rHealth :: !Int
    , rAttack :: !Int
    , rDefence :: !Int
    } deriving (Show)

calcStats :: RatHull -> (Int, Int, Int)
calcStats = mStats . fmap getSum . foldMap (fmap Sum)

animate :: RatHull -> Maybe Rat
animate rh
    | length rh < 3 = Nothing
    | otherwise =
        let (h, a, d) = calcStats rh
        in Just $ Rat rh h a d

isEnoughFor :: Int -> RatHull -> Bool
isEnoughFor energy = (<= energy) . sum . fmap sum
