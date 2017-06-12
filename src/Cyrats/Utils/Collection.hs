{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | A HashMap based storage for domain objects
module Cyrats.Utils.Collection
      -- * types
    ( Collection()
    , Key
      -- * constructors
    , empty
    , fromList
      -- * operations
    , insert
    , killAt
    , like
    , modifyAt
    , toList
    ) where

import Control.Lens hiding (like)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.List as L

import Cyrats.Utils.Except

type Key = Int

data Collection a = Collection
    { _cSeq :: Key
    , _cItems :: HashMap Key a
    } deriving (Show)

makeLenses ''Collection

insert :: a -> Collection a -> Collection a
insert x c = c & cItems %~ M.insert (c ^. cSeq) x & cSeq %~ (+ 1)

killAt :: Key -> Collection a -> Possible (a, Collection a)
killAt k = modify k $ \v c -> pure (v, c & cItems %~ M.delete k)

modifyAt :: Key -> (a -> Possible a) -> Collection a -> Possible (Collection a)
modifyAt k f =
    modify k $ \v c -> do
        v' <- f v
        pure $ c & cItems %~ M.insert k v'

modify :: Key -> (a -> Collection a -> Possible b) -> Collection a -> Possible b
modify k next c = orThrow "Bad key!" (M.lookup k $ c ^. cItems) >>= flip next c

toList :: Collection a -> [(Key, a)]
toList = view $ cItems . to M.toList

fromList :: [a] -> Collection a
fromList = Collection 0 . M.fromList . zip [0 ..]

empty :: Collection a
empty = Collection 0 M.empty

-- | Just a naive test for similarity (for testing)
like
    :: Ord a
    => Collection a -> Collection a -> Bool
like c1 c2 =
    let unpack = L.sort . map snd . M.toList . view cItems
    in unpack c1 == unpack c2
