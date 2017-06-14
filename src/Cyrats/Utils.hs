module Cyrats.Utils
    ( module X
    , bindAll
    ) where

import Data.List
import Data.Traversable

import Cyrats.Utils.Collection as X
import Cyrats.Utils.Except as X

bindAll
    :: (Traversable t, Monad m)
    => t (a -> m a) -> a -> m a
bindAll fs x = foldl' (>>=) (pure x) fs
