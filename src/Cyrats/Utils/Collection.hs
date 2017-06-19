{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | A HashMap based storage for domain objects
module Cyrats.Utils.Collection
      -- * types
    ( Collection()
      -- * constructors
    , fromListOf
      -- * optics
    , keys
    , values
      -- * operations
    , insert
    , killAt
    , modifyAt
    , toList
    ) where

import Control.Lens hiding (like)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Cyrats.Utils.Except

type KeyIso k = Iso' k Int

data Collection k a = Collection
    { _cSeq :: Int
    , cKeyIso :: KeyIso k
    , _cItems :: HashMap Int a
    }

makeLenses ''Collection

instance Show a =>
         Show (Collection k a) where
    show c = "Collection {" ++ show (c ^. cItems) ++ "}"

insert :: a -> Collection k a -> Collection k a
insert x c = c & cItems %~ M.insert (c ^. cSeq) x & cSeq %~ (+ 1)

killAt :: k -> Collection k a -> Possible (a, Collection k a)
killAt k = modify k $ \k' c v -> pure (v, c & cItems %~ M.delete k')

modifyAt :: k
         -> (a -> Possible a)
         -> Collection k a
         -> Possible (Collection k a)
modifyAt k f =
    modify k $ \k' c v -> do
        v' <- f v
        pure $ c & cItems %~ M.insert k' v'

modify :: k
       -> (Int -> Collection k a -> a -> Possible b)
       -> Collection k a
       -> Possible b
modify k next c = orThrow "Bad key!" (M.lookup key $ c ^. cItems) >>= next key c
  where
    key = k ^. cKeyIso c

toList :: Collection k a -> [(k, a)]
toList c =
    view (cItems . to M.toList . to (map . over _1 . review $ cKeyIso c)) c

fromListOf :: KeyIso k -> [a] -> Collection k a
fromListOf i = Collection 0 i . M.fromList . zip [0 ..]

keys :: Getter (Collection k a) [k]
keys = to toList . to (map fst)

values :: Getter (Collection k a) [a]
values = to toList . to (map snd)
