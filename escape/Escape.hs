module Main where

import Agent
import Direction
import Maze

import           Control.Monad.ST
import           Data.STRef                (newSTRef)
import           Data.Set                  (Set, (\\))
import qualified Data.Set as S
import           Data.Text                 (Text)
import qualified Data.Text.IO as T
import           Prelude            hiding (Left, Right)
import           System.Environment        (getArgs)
import           System.Random             (getStdGen)

newtype AgentId =
    AgentId Text

-- What the user gets back
data Sensory =
    Sensory { availableMoves :: ![Direction]
            , onExit         :: !Bool
            }

data AgentCommand =
    AgentCommand AgentId Command

-- What the user submits
data Command = Exit
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

    printMaze maze exit agent

    step (Solver mempty mempty mempty) maze exit agent

printMaze :: Maze -> (Int, Int) -> Agent -> IO ()
printMaze maze (ex, ey) (Agent (ax, ay)) =
    T.putStrLn $ pretty [(ax,ay,'a'), (ex,ey,'e')] maze



{- 
    Solver:
-}

data Solver = 
    Solver { fringe  :: !(Set (Int, Int))
           , visited :: !(Set (Int, Int))
           , path    :: ![(Int, Int)]
           } deriving Show

step solver maze exit (Agent (x, y))

    | exit == (x, y) = print "Done"

    | otherwise = do

        print solver

        let allowedMoves = permitteds' maze (x, y) \\ visited solver
            fringe'      = S.delete (x, y) (fringe solver) <> allowedMoves
            visited'     = S.insert (x, y) (visited solver)

        case S.toList allowedMoves of

            [] ->

                let (p:ps) = path solver

                    solver' = Solver { fringe  = fringe'
                                     , visited = visited'
                                     , path    = ps
                                     }

                in step solver' maze exit (Agent p)

            (a:_) ->

                let solver' = Solver { fringe  = fringe'
                                     , visited = visited'
                                     , path    = a : path solver
                                     }
                in step solver' maze exit (Agent a)