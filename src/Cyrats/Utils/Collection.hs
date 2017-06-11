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
    , modifyAt
    , toList
    ) where

import Control.Lens
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Hashable

import Cyrats.Utils.Except

type Key = Int

newtype Collection a = Collection
    { _unCollection :: (HashMap Key a)
    } deriving (Show, Eq)

makeLenses ''Collection

insert
    :: Hashable a
    => a -> Collection a -> Collection a
insert x = over unCollection $ M.insert (hash x) x

killAt :: Key -> Collection a -> Possible (a, Collection a)
killAt k = modify k $ \v c -> pure (v, c & unCollection %~ M.delete k)

modifyAt :: Key -> (a -> Possible a) -> Collection a -> Possible (Collection a)
modifyAt k f =
    modify k $ \v c -> do
        v' <- f v
        pure $ c & unCollection %~ M.insert k v'

modify :: Key -> (a -> Collection a -> Possible b) -> Collection a -> Possible b
modify k next c = do
    orThrow "Bad key!" (M.lookup k $ c ^. unCollection) >>= flip next c

toList :: Collection a -> [(Key, a)]
toList = view $ unCollection . to M.toList

fromList
    :: Hashable a
    => [a] -> Collection a
fromList = Collection . M.fromList . Prelude.map (\x -> (hash x, x))

empty :: Collection a
empty = Collection M.empty
