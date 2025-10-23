module Control.Memoization where

import Control.Monad.State (State)
import Data.Map            (Map)

-- | The memoization type.
type M s a = M' (Map s a) a

-- | A general memoization type.
type M' s a = State s a