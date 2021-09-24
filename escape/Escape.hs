module Main where

import Agent
import Direction
import Maze

import           Control.Monad.ST
import           Data.STRef                (newSTRef)
import qualified Data.Text.IO as T
import           Prelude            hiding (Left, Right)
import           System.Environment        (getArgs)
import           System.Random             (getStdGen)

newtype AgentId =
    AgentId String

data Sensory =
    Sensory { moved    :: !Bool
            , onExit   :: !Bool
            , onAgents :: ![AgentId]
            }

data Command = Exit
             | Wait
             | Go !Direction

main :: IO ()
main = do

    [fname] <- getArgs

    maze <- read <$> readFile fname

    T.putStrLn $ pretty [] maze

    stdgen <- getStdGen

    let (agent, exit) = runST $ do
            gen   <- newSTRef stdgen
            agent <- Agent <$> randomBoardPosition maze gen
            exit  <- randomBoardPosition maze gen
            pure (agent, exit)

    find maze exit agent

    where
    find maze exit agent = do

        printMaze maze exit agent

        let allowedMoves = let Agent (x, y) = agent
                           in permitteds maze (x, y)

        print allowedMoves

        pure ()

printMaze :: Maze -> (Int, Int) -> Agent -> IO ()
printMaze maze (ex, ey) (Agent (ax, ay)) =
    T.putStrLn $ pretty [(ax,ay,'a'), (ex,ey,'e')] maze
