import System.IO
import Data.List
import Data.List.Split
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
reduce inp = if reduced == inp then inp else reduce' reduced
    where reduced = reduce' inp

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let parsed = map parse ls
    let allergens = Set.toList $ foldl1 (Set.union) (map (Set.fromList . snd) parsed)
    let candidates = map (\a -> (a, findAllergenCandidates parsed a)) allergens
    let finalCandidates = reduce candidates
    let allSuspectedIngredients = Set.fromList $ concat $ map snd finalCandidates
    print $ allSuspectedIngredients
    let allNonAllergic = concat $ map (\x -> Set.toList $ Set.difference ((Set.fromList . fst) x) allSuspectedIngredients) parsed
    print $ allNonAllergic
    print $ length allNonAllergic

main = withFile "input/Day21.txt" ReadMode run