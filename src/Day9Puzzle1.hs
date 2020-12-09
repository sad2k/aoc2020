import System.IO

findSolution' prev (x:xs)
    | allowed = findSolution' ((drop 1 prev) ++ [x]) xs
    | otherwise = x
        where allowed = not $ null [p1+p2 | p1 <- prev, p2 <- prev, p1+p2 == x]

findSolution :: [Int] -> Int -> Int
findSolution xs n = findSolution' (take n xs) (drop n xs)

run h = do
    contents <- hGetContents h
    print $ findSolution (map read $ lines contents) 25

main = withFile "input/Day9.txt" ReadMode run