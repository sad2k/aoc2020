import System.IO
import Data.List.Split
import qualified Data.Set as Set

uniqueAnswers xs = Set.toList $ foldl (\s ch -> Set.insert ch s) Set.empty (concat xs)

run h = do
    contents <- hGetContents h
    print $ sum $ map (length . uniqueAnswers) $ splitOn [""] $ lines contents

main = withFile "input/Day6.txt" ReadMode run