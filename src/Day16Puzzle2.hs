import System.IO
import Data.List
import Data.List.Split
import Debug.Trace (trace)

data Def = MkDef {
    name :: String,
    rules :: [(Int,Int)]
} deriving Show

parseDef :: String -> Def
parseDef x = MkDef name szs
    where [name,rest] = splitOn ":" x
          szs = map parseSz (splitOn " or " rest)
          parseSz x = case (splitOn "-" x) of
            [v1,v2] -> (read v1, read v2)

parseList :: String -> [Int]
parseList xs = map read (splitOn "," xs)

findInvalid :: [Int] -> [Def] -> [Int]
findInvalid xs defs = filter isInvalid xs
    where isInvalid x = all (\d -> allRulesInvalid d x) defs
          allRulesInvalid d x = all (\r -> x < fst r || x > snd r) (rules d)

isValid :: Def -> Int -> Bool
isValid def x = any (\r -> x >= fst r && x <= snd r) (rules def)

doesDefMatchAllExamples :: Def -> [Int] -> Bool
doesDefMatchAllExamples def examples = all f examples
    where f ex = isValid def ex

findCandidates :: [Def] -> [[Int]] -> [[String]] -> [[String]]
findCandidates defs [] acc = acc
findCandidates defs (field:fields) acc = findCandidates defs fields (acc ++ [map name allowedDefs])
    where allowedDefs = filter f defs
          f d = doesDefMatchAllExamples d field

exclude :: [String] -> [String] -> [String]
exclude xs excluded = filter (\x -> not (x `elem` excluded)) xs

reduce' :: [[String]] -> [[String]]
reduce' xs = map excl xs
    where singletons = concat $ filter (\x -> length x == 1) xs
          excl [x] = [x]
          excl xs = exclude xs singletons

reduce :: [[String]] -> [[String]]
reduce xs = if xs == reduced then reduced else (reduce reduced)
    where reduced = reduce' xs

run h = do
    contents <- hGetContents h
    let [defs, your, nearby] = splitOn [""] $ lines contents
    let your' = head $ map parseList $ drop 1 your
    let nearby' = map parseList $ drop 1 nearby
    let parsedDefs = map parseDef defs
    let validNearby = filter (\xs -> length (findInvalid xs parsedDefs) == 0) nearby'
    let cand = findCandidates parsedDefs (transpose validNearby) []
    let reduced = zip (concat $ reduce cand) [0..]
    let departures = filter (\x -> (take 9 (fst x)) == "departure") reduced
    let res = map (\x -> your' !! (snd x)) departures
    print $ product res

main = withFile "input/Day16.txt" ReadMode run