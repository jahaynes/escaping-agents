module Rand where

import Control.Monad.ST (ST)
import Data.STRef       (STRef, readSTRef, writeSTRef)
import System.Random    (Random(..), StdGen)

rand :: Random a => (a, a)
                 -> STRef s StdGen
                 -> ST s a
rand range gen = do
    (a, g) <- randomR range <$> readSTRef gen
    gen `writeSTRef` g
    pure a