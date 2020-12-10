import System.IO
import Debug.Trace (trace)
import Data.Sort
import qualified Data.Map as Map

findSolution' :: [Int] -> [(Int,Int)] -> [(Int, Int)]
findSolution' [] path = path
findSolution' (x:xs) path = findSolution' xs ((x, candsum):path)
    where candidates = takeWhile (\p -> (x-(fst p)) <= 3) path
          candsum = sum (map snd candidates)

findSolution :: [Int] -> [(Int,Int)]
findSolution xs = findSolution' sorted [(0,1)]
    where sorted = sort xs

run h = do
    contents <- hGetContents h
    let sol = findSolution (map read $ lines contents)
    print $ snd (head sol)

main = withFile "input/Day10.txt" ReadMode run
