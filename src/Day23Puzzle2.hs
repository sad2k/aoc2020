import Data.List
import Data.List.Split
import System.TimeIt
import Data.Maybe
import Debug.Trace (trace)
import qualified Data.Map as Map

type GameState = Map.Map Int Int

maxNumber :: Int
maxNumber = 1000000

findDestination :: Int -> [Int] -> Int
findDestination 0 taken = findDestination maxNumber taken
findDestination n taken = if n `elem` taken then findDestination (n-1) taken else n

playRound :: (GameState, Int) -> (GameState, Int)
playRound (st,cur) = (updatedState, newCur)
    where taken1 = fromJust $ Map.lookup cur st
          taken2 = fromJust $ Map.lookup taken1 st
          taken3 = fromJust $ Map.lookup taken2 st
          newCur = fromJust $ Map.lookup taken3 st
          dest = findDestination (cur-1) [taken1, taken2, taken3]
          oldAfterDest = fromJust $ Map.lookup dest st
          updatedState = Map.insert dest taken1 $ Map.insert taken3 oldAfterDest $ Map.insert cur newCur $ st

printFromOne' :: GameState -> Int -> [Int] -> [Int]
printFromOne' st 1 acc = acc
printFromOne' st k acc = printFromOne' st (fromJust $ Map.lookup k st) (acc ++ [k])

printFromOne :: GameState -> [Int]
printFromOne st = printFromOne' st (fromJust $ Map.lookup 1 st) []

take2AfterOne :: GameState -> [Int]
take2AfterOne st = [first, second]
    where first = fromJust $ Map.lookup 1 st
          second = fromJust $ Map.lookup first st

parseInput :: String -> [Int]
parseInput input = initial ++ [((maximum initial)+1)..1000000]
    where initial = map (\ch -> (read [ch]) :: Int) input

findSolution = do
    let input = "253149867"
    let parsedInput = parseInput input
    let inputMap = Map.fromList $ zip parsedInput ((drop 1 parsedInput) ++ ([head parsedInput]))
    let finalState = last $ take 10000001 $ iterate playRound (inputMap, (head parsedInput))
    let result = take2AfterOne $ fst finalState
    print $ result
    print $ product result

main = timeIt findSolution


