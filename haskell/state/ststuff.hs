module Blah where

import Control.Monad.ST
import Data.STRef

data IntRef = IntRef (forall s. ST s (STRef s Int))

--data IntRef = IntRef (forall s. STRef s Int)

--makeRef = IntRef $ newSTRef 4

getRefVal (IntRef state) = do
  r <- state
  readSTRef r

--getRefVal (IntRef r) = do readSTRef r

blah = runST(getRefVal (IntRef (newSTRef 4)))
