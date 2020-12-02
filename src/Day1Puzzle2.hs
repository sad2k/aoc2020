
import System.IO

findSolution :: [Int] -> Int
findSolution l = head [x*y*z | x <- l, y <- l, z <- l, x<y, y<z, x+y+z == 2020]

run h = do
    contents <- hGetContents h
    print $ findSolution $ map read $ lines contents

main = withFile "input/Day1.txt" ReadMode run



