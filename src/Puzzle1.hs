
import System.Directory
import System.IO

findSolution :: [Int] -> Int
findSolution l = head [x*y | x <- l, y <- l, x<y, x+y == 2020]

run h = do
    contents <- hGetContents h
    print $ findSolution $ map read $ lines contents

main = withFile "input/Puzzle1.txt" ReadMode run

