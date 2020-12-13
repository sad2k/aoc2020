import System.IO
import Debug.Trace (trace)
import Data.List
import Data.List.Split

findSolution :: [Int] -> Int -> (Int,Int)
findSolution buses ts = case findBus of
    Just bus -> (bus,ts)
    Nothing -> findSolution buses (ts+1)
    where findBus = find (\x -> ts `mod` x == 0) buses

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let ts = read (head ls)
    let buses = map read $ filter (/= "x") $ splitOn "," ((head . tail) ls)
    let (bus, busTs) = findSolution buses ts
    print $ bus * (busTs - ts)

main = withFile "input/Day13.txt" ReadMode run