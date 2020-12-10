import System.IO
import Debug.Trace (trace)
import Data.Sort
import qualified Data.Map as Map

findSolution' :: [Int] -> Int -> Map.Map Int Int -> Map.Map Int Int
findSolution' [] _ acc = acc
findSolution' (x:xs) cur acc
    | (x-cur) > 3 = error ("unexpected value: " ++ show x)
    | otherwise = findSolution' xs x (Map.insertWith (+) (x-cur) 1 acc)

findSolution :: [Int] -> Map.Map Int Int
findSolution xs = findSolution' (sorted ++ [(maximum sorted)+3]) 0 Map.empty
    where sorted = sort xs

run h = do
    contents <- hGetContents h
    let sol = findSolution (map read $ lines contents)
    print sol
    print $ (sol Map.! 1) * (sol Map.! 3)

main = withFile "input/Day10.txt" ReadMode run
