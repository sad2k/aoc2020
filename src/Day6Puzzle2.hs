import System.IO
import Data.List.Split
import qualified Data.Set as Set

uniqueAnswers (x:[]) = x
uniqueAnswers xs = foldl1 f xs
    where f v1 v2 = Set.toList $ (Set.fromList v1) `Set.intersection` (Set.fromList v2)

run h = do
    contents <- hGetContents h
    print $ sum $ map (length . uniqueAnswers) $ splitOn [""] $ lines contents

main = withFile "input/Day6.txt" ReadMode run