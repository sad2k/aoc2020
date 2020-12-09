import System.IO
import Debug.Trace (trace)

findSolution' prev (x:xs)
    | allowed = findSolution' ((drop 1 prev) ++ [x]) xs
    | otherwise = x
        where allowed = not $ null [p1+p2 | p1 <- prev, p2 <- prev, p1+p2 == x]

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = if p x then [x] else (x:(takeUntil p xs))

findSolution :: [Int] -> Int -> Int
findSolution xs n
    | not (null s) = minimum s + maximum s
    | otherwise = findSolution (drop 1 xs) n
        where cumsum = takeUntil (\x -> x >= n) (scanl1 (+) xs)
              s = if (last cumsum == n) then (take (length cumsum) xs) else []

run h = do
    contents <- hGetContents h
    print $ findSolution (map read $ lines contents) 36845998

main = withFile "input/Day9.txt" ReadMode run