module Agent where

newtype Agent =
    Agent { pos :: (Int, Int) }
        deriving Show
