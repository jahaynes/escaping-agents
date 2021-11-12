module Main where

import Agent
import Direction
import Maze

import           Control.Monad.ST
import           Data.List                 (minimumBy)
import           Data.Ord                  (comparing)
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
            age <- randomBoardPosition maze gen
            exi  <- randomBoardPosition maze gen
            pure (age, exi)

    printMaze maze exit (Agent agent)

    step maze exit (Solver (S.singleton agent) [agent])

printMaze :: Maze -> (Int, Int) -> Agent -> IO ()
printMaze maze (ex, ey) (Agent (ax, ay)) =
    T.putStrLn $ pretty [(ax,ay,'a'), (ex,ey,'e')] maze

data Solver = 
    Solver { visited :: !(Set (Int, Int))
           , path    :: ![(Int, Int)]
           } deriving Show

data Decision = Done
              | Backtrack
              | MoveTo !(Int, Int)

step :: Maze -> (Int, Int) -> Solver -> IO ()
step maze exit@(ex, ey) = go

    where
    go solver =

        case decision of

            Done ->
                print "Done"

            Backtrack -> do
                print "Backtrack"
                let solver' = solver { path = tail (path solver) }
                go solver'

            MoveTo c -> do
                print $ "Move to " <> show c
                let solver' = solver { visited = S.insert c (visited solver)
                                    , path    = c : path solver }
                go solver'

        where
        decision

            | null (path solver) = error "No solution"

            -- Are we done
            | head (path solver) == exit = Done

            -- pick the best unvisited adjacent cell (or backtrack)
            | otherwise =

                case best (unvisited adjacentCells) of

                    Nothing -> Backtrack

                    Just c  -> MoveTo c

            where
            best :: Set (Int, Int) -> Maybe (Int, Int)
            best cells | S.null cells = Nothing
                       | otherwise    = Just . minimumBy (comparing distanceFromExit) $ S.toList cells

            unvisited :: Set (Int, Int) -> Set (Int, Int)
            unvisited cells = cells \\ visited solver

            adjacentCells :: Set (Int, Int)
            adjacentCells = permitteds' maze . head $ path solver

            distanceFromExit :: (Int, Int) -> Int
            distanceFromExit (x, y) = abs (ex - x) + abs (ey - y)
