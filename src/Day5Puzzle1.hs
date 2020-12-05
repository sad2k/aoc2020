import System.IO

mid (low,high) = (high + low + 1) `div` 2

calculateRow s = fst $ foldl f (0,127) s
    where f acc 'F' = (fst acc, (mid acc-1))
          f acc 'B' = (mid acc, snd acc)

calculateCol s = fst $ foldl f (0,7) s
    where f acc 'L' = (fst acc, (mid acc-1))
          f acc 'R' = (mid acc, snd acc)

calculateSeat s = (calculateRow (take 7 s) * 8) + calculateCol (drop 7 s)

run h = do
    contents <- hGetContents h
    print $ maximum $ map calculateSeat $ lines contents

main = withFile "input/Day5.txt" ReadMode run

