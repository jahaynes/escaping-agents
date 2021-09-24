module Agent where

import Maze

data Agent =
    Agent { pos :: !(Int, Int) }
        deriving Show

moves :: Agent
      -> Maze
      -> [(Int, Int)]
moves agent maze = undefined