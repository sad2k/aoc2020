import System.IO
import Data.List.Split

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

run h = do
    contents <- hGetContents h
    let [defs, your, nearby] = splitOn [""] $ lines contents
    let your' = map parseList $ drop 1 your
    let nearby' = map parseList $ drop 1 nearby
    let parsedDefs = map parseDef defs
    let invalid = concat $ map (\xs -> findInvalid xs parsedDefs) nearby'
    print $ sum invalid

main = withFile "input/Day16.txt" ReadMode run