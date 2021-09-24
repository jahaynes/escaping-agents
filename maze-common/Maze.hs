{-# LANGUAGE OverloadedStrings #-}

module Maze where

import Direction
import Rand

import Control.Monad         (forM_)
import Control.Monad.ST      (ST)
import Control.Monad.Writer  (Writer, execWriter, tell)
import Data.Array            (Array, (!), bounds)
import Data.Bool             (bool)
import Data.STRef            (STRef)
import Data.Set              (Set)
import qualified Data.Set as S
import Data.Text             (Text, singleton)
import Prelude        hiding (Left, Right)
import Safe                  (headMay)
import System.Random         (StdGen)

data Maze = Maze { rightWalls :: !(Array (Int, Int) Bool)
                 , belowWalls :: !(Array (Int, Int) Bool)
                 } deriving (Show, Read)

randomBoardPosition :: Maze
                    -> STRef s StdGen
                    -> ST s (Int, Int)
randomBoardPosition maze gen = (,) <$> rand (0, getMaxX maze) gen
                                   <*> rand (0, getMaxY maze) gen

permitteds :: Maze
           -> (Int, Int)
           -> [Direction]
permitteds maze (x, y) =
    concat [ if y > 0            && not (belowWalls maze ! (x, y-1)) then    [Up] else []
           , if y < getMaxY maze && not (belowWalls maze ! (x,   y)) then  [Down] else []
           , if x > 0            && not (rightWalls maze ! (x-1, y)) then  [Left] else []
           , if x < getMaxX maze && not (rightWalls maze ! (x,   y)) then [Right] else []
           ]

permitteds' :: Maze
            -> (Int, Int)
            -> Set (Int, Int)
permitteds' maze (x, y) =
    mconcat [ if y > 0            && not (belowWalls maze ! (x, y-1)) then S.singleton (x, y-1) else mempty
            , if y < getMaxY maze && not (belowWalls maze ! (x,   y)) then S.singleton (x, y+1) else mempty
            , if x > 0            && not (rightWalls maze ! (x-1, y)) then S.singleton (x-1, y) else mempty
            , if x < getMaxX maze && not (rightWalls maze ! (x,   y)) then S.singleton (x+1, y) else mempty
            ]

{-
permitted :: Maze
          -> (Int, Int)
          -> Direction
          -> Bool
permitted maze (x, y) dir =

    case dir of

        Up    -> y > 0
              && not (belowWalls maze ! (x, y-1))

        Down  -> y < getMaxY maze
              && not (belowWalls maze ! (x, y))

        Left  -> x > 0
              && not (rightWalls maze ! (x-1, y))

        Right -> x < getMaxX maze
              && not (rightWalls maze ! (x, y))
-}

getMaxX :: Maze -> Int
getMaxX = fst . snd . bounds . rightWalls

getMaxY :: Maze -> Int
getMaxY = snd . snd . bounds . rightWalls

--neighbors :: Maze -> (Int, Int) -> [(Int, Int)]
--neighbors maze (x, y) = neighbors' (getMaxX maze, getMaxY maze) (x, y)

neighbors' :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbors' (maxX, maxY) (x, y) = bool [(x - 1, y)] [] (0 == x)
                              ++ bool [(x + 1, y)] [] (maxX == x)
                              ++ bool [(x, y - 1)] [] (0 == y)
                              ++ bool [(x, y + 1)] [] (maxY == y)

pretty :: [(Int, Int, Char)] -> Maze -> Text
pretty pois = execWriter . prettyWriter

    where
    prettyWriter :: Maze -> Writer Text ()
    prettyWriter maze@(Maze rWalls bWalls) = do
        tell "+"
        tell $ mconcat (replicate (getMaxX maze + 1) "---+")
        tell "\n"
        forM_ [0 .. getMaxY maze] $ \y -> do
            tell "|"
            forM_ [0 .. getMaxX maze] $ \x -> do
                case headMay . map (\(_,_,c) -> c)
                             . filter (\(px,py,_) -> px == x && py == y)
                             $ pois of
                    Just c   -> tell " " >> tell (singleton c) >> tell " "
                    Nothing  -> tell "   "
                tell $ bool " " "|" (rWalls ! (x, y))
            tell "\n"
            forM_ [0 .. getMaxX maze] $ \x -> do
                tell "+"
                tell $ bool "   " "---" (bWalls ! (x, y))
            tell "+\n"
