-- | Extra stuff for Control.Monad.Except
module Cyrats.Utils.Except
    ( Possible
      -- * helpers
    , orThrow
      -- * reexports
    , Text
    ) where

import Control.Monad.Except
import Data.Text

type Possible = Except Text

orThrow :: Text -> Maybe a -> Except Text a
orThrow msg = maybe (throwError msg) pure
