import System.IO
import Debug.Trace (trace)
import Data.Sort
import qualified Data.Map as Map

findSolution' :: [Int] -> [Int] -> [[Int]]
findSolution' [] path = [path]
findSolution' xs path = concat $ map (\c -> findSolution' (drop (snd c) xs) ((fst c):path)) candidates
    where l = head path
          candidates = zip (takeWhile (\x -> (x-l) <= 3) xs) [1..]

findSolution :: [Int] -> [[Int]]
findSolution xs = findSolution' (sorted ++ [(maximum sorted)+3]) [0]
    where sorted = sort xs

run h = do
    contents <- hGetContents h
    let sol = findSolution (map read $ lines contents)
--     mapM print sol
    print $ length sol

main = withFile "input/Day10.txt" ReadMode run
