import System.IO
import Data.List
import Debug.Trace (trace)

sliding :: [a] -> Int -> [[a]]
sliding l n = map (take n) (take chunks (tails l))
    where chunks = length l - n + 1

transform' s b a = map f (sliding zipped 3)
    where zipped = (('.','.','.'):(zip3 b s a)) ++ [('.','.','.')]
          f [p1, (cb,cc,ca), p2] = case cc of
            'L' -> if (occupied p1 p2 cb ca == 0) then '#' else 'L'
            '#' -> if (occupied p1 p2 cb ca >= 4) then 'L' else '#'
            ch -> ch
          occupied (s1,s2,s3) (s4,s5,s6) s7 s8 = length $ filter (=='#') [s1,s2,s3,s4,s5,s6,s7,s8]

transform :: [String] -> [String]
transform xs = map f (sliding padded 3)
    where padded = (floor:xs) ++ [floor]
          f (b:s:a:[]) = transform' s b a
          floor = replicate (length (head xs)) '.'

findSolution :: [String] -> [[String]]
findSolution xs = if (xs == tr) then [tr] else tr:(findSolution tr)
    where tr = transform xs

run h = do
    contents <- hGetContents h
    let sol = findSolution (lines contents)
    mapM print sol
    print $ (length . filter (=='#') . concat . last) sol

main = withFile "input/Day11.txt" ReadMode run
