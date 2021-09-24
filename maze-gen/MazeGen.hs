{-# LANGUAGE RankNTypes, TypeFamilies #-}

module Main where

import Maze
import Rand

import           Control.Monad    (forM_, unless)
import           Control.Monad.ST (ST, runST)
import           Data.Array.ST    (STArray, freeze, newArray, readArray, writeArray)
import           Data.STRef       (STRef, newSTRef)
import qualified Data.Text.IO as T
import           System.Random    (StdGen, getStdGen)

genMaze :: Int
        -> Int
        -> StdGen
        -> ST s Maze
genMaze width height stdgen = do

    visited <- freshArray False
    rWalls  <- freshArray True
    bWalls  <- freshArray True
    gen     <- newSTRef stdgen

    start <- (,) <$> rand (0, maxX) gen
                 <*> rand (0, maxY) gen

    visit gen visited rWalls bWalls start

    Maze <$> freeze rWalls
         <*> freeze bWalls

    where
    visit :: forall s. STRef s StdGen 
                    -> STArray s (Int, Int) Bool
                    -> STArray s (Int, Int) Bool
                    -> STArray s (Int, Int) Bool
                    -> (Int, Int)
                    -> ST s ()
    visit gen visited rWalls bWalls here = do

        writeArray visited here True
        let ns = neighbors' (maxX, maxY) here
        i <- rand (0, length ns - 1) gen
        forM_ (ns !! i : take i ns ++ drop (i + 1) ns) $ \there -> do
            seen <- readArray visited there
            unless seen $ do
                removeWall here there
                visit gen visited rWalls bWalls there

        where
        removeWall (x1, y1) (x2, y2) = do
            let wallType = if x1 == x2 then bWalls else rWalls
            writeArray wallType (min x1 x2, min y1 y2) False

    maxX :: Int
    maxX = width - 1

    maxY :: Int
    maxY = height - 1

    freshArray :: Bool -> ST s (STArray s (Int, Int) Bool)
    freshArray = newArray ((0, 0), (maxX, maxY)) 
 
main :: IO ()
main = do 

    gen <- getStdGen

    let maze = runST $ genMaze 4 3 gen

    writeFile "maze_01" (show maze)

    T.putStrLn $ pretty [] maze

    putStrLn "Maze written"