module Control.Memoization where

import Control.Monad.State (State)
import Data.Map            (Map)

-- | The memoization type.
type M s a = State (Map s a) a