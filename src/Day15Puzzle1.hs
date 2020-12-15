import System.IO
import Data.List.Split
import qualified Data.Map as Map

data GameState = MkGameState {
    turns :: Map.Map Int [Int],
    lastTurnValue :: Int,
    lastTurnNumber :: Int
} deriving Show

updateState :: GameState -> Int -> GameState
updateState st num = MkGameState newTurns num (lastTurn+1)
    where newTurns = Map.insertWith (\l1 l2 -> take 2 ((head l1):l2)) num [lastTurn+1] (turns st)
          lastTurn = lastTurnNumber st

gameSeq :: [Int] -> GameState -> [Int]
gameSeq [] st = turn:(gameSeq [] (updateState st turn))
    where t = turns st
          l = lastTurnValue st
          turn = case (Map.lookup l t) of
            Just [_] -> 0
            Just [lst,prv] -> lst-prv
gameSeq (x:xs) st = x:(gameSeq xs (updateState st x))

run h = do
    contents <- hGetContents h
    let nums = map read $ splitOn "," $ head (lines contents)
    print (nums :: [Int])
    let startState = MkGameState Map.empty 0 0
    let seq = take 2020 $ gameSeq nums startState
    print $ (head . reverse) seq

main = withFile "input/Day15.txt" ReadMode run