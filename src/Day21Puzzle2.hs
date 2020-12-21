import System.IO
import Data.List
import Data.List.Split
import Data.Sort
import qualified Data.Set as Set

parse :: String -> ([String], [String])
parse s = (ingredients, allergens)
    where spl = splitOn " (contains " s
          ingredients = splitOn " " (head spl)
          allergens = splitOn ", " (init $ head $ drop 1 $ spl)

findAllergenCandidates :: [([String], [String])] -> String -> [String]
findAllergenCandidates inp allergen = Set.toList $ foldl1 (Set.intersection) fltIng
    where fltIng = map (Set.fromList . fst) $ filter (\p -> allergen `elem` snd p) inp

reduce' :: [(String, [String])] -> [(String, [String])]
reduce' inp = removeDefinite
    where removeDefinite = map removeFromOne inp
          removeFromOne (id, xs) = if length xs == 1 then (id,xs) else (id, Set.toList $ Set.difference (Set.fromList xs) definite)
          definite = Set.fromList $ concat $ map snd $ filter (\p -> length (snd p) == 1) inp

reduce :: [(String, [String])] -> [(String, [String])]
reduce inp = if reduced == inp then inp else reduce reduced
    where reduced = reduce' inp

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let parsed = map parse ls
    let allergens = Set.toList $ foldl1 (Set.union) (map (Set.fromList . snd) parsed)
    let candidates = map (\a -> (a, findAllergenCandidates parsed a)) allergens
    let finalCandidates = reduce candidates
    print $ intercalate "," $ concat $ map snd $ sortBy (\p1 p2 -> fst p1 `compare` fst p2) finalCandidates

main = withFile "input/Day21.txt" ReadMode run