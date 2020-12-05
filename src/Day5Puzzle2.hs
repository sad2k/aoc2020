import System.IO
import Data.Sort
import Data.List
import Data.Maybe

mid (low,high) = (high + low + 1) `div` 2

calculateRow s = fst $ foldl f (0,127) s
    where f acc 'F' = (fst acc, (mid acc-1))
          f acc 'B' = (mid acc, snd acc)

calculateCol s = fst $ foldl f (0,7) s
    where f acc 'L' = (fst acc, (mid acc-1))
          f acc 'R' = (mid acc, snd acc)

calculateSeat s = (calculateRow (take 7 s) * 8) + calculateCol (drop 7 s)

findGap l = before + 1
    where (after, before) = fromJust (find pred $ zip (drop 1 l) l)
          pred (v,prev) = v - prev > 1

run h = do
    contents <- hGetContents h
    print $ findGap $ sort $ map calculateSeat $ lines contents

main = withFile "input/Day5.txt" ReadMode run

