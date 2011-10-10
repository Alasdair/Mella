module Lang.Util.Counter where

import Control.Monad.Trans.State

newtype Counter = Counter { counterToInt :: Int }

count :: Counter -> (Int, Counter)
count (Counter n) = (n, Counter (n + 1))

