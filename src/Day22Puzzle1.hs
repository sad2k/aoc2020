import Data.List.Split
import Data.Sort
import System.IO

getCards :: [String] -> [[Int]]
getCards xs = map (map read . drop 1) (splitOn [""] xs)

playRound :: [[Int]] -> [[Int]]
playRound players = map addIfWinner (zip players [0..])
    where winner = (snd . head) $ sortBy (\p1 p2 -> fst p2 `compare` fst p1) highest
          highest = zip (map head players) [0..]
          addIfWinner (l,id) = if id == winner then (drop 1 l) ++ (reverse $ sort $ map fst highest) else (drop 1 l)

play :: [[Int]] -> [[Int]]
play players = if (any null players) then players else play round
    where round = playRound players

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let players = getCards ls
    let result = head $ filter (not . null) $ play $ getCards ls
    print $ sum $ map (\p -> fst p * snd p) $ zip result [length result,((length result)-1)..1]

main = withFile "input/Day22.txt" ReadMode run