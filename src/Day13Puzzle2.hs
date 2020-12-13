import System.IO
import Debug.Trace (trace)
import Data.List
import Data.List.Split

findSolution :: [(Integer,Integer)] -> Integer -> Integer -> Integer
findSolution [] ts offset = ts
findSolution ((busId,busOffset):buses) ts offset = if ((ts+busOffset) `mod` busId == 0)
    then findSolution buses ts (offset*busId)
    else findSolution ((busId,busOffset):buses) (ts+offset) offset

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let buses = zip (splitOn "," ((head . tail) ls)) [0..]
    let relevantBuses = map (\x -> (read (fst x), snd x)) (filter (\x -> fst x /= "x") buses)
    let firstBusNum = fst (head relevantBuses)
    let primeProduct = product $ map fst relevantBuses
    let sol = findSolution relevantBuses 0 1
    print sol

main = withFile "input/Day13.txt" ReadMode run