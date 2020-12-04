import System.IO
import Data.List.Split
import qualified Data.Map as Map

parsePassportLine :: String -> Map.Map String String -> Map.Map String String
parsePassportLine l m = foldr insertIntoMap m entries
    where entries = map (splitOn ":") (splitOn " " l)
          insertIntoMap (k:v:[]) map = Map.insert k v map

isValidPassport :: [String] -> Bool
isValidPassport ls = checkMap $ foldr parsePassportLine Map.empty ls
    where requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
          checkMap :: Map.Map String String -> Bool
          checkMap m = all (\k -> Map.member k m) requiredKeys

findSolution :: [[String]] -> Int
findSolution ls = (length . filter id . map isValidPassport) ls

run h = do
    contents <- hGetContents h
    print $ findSolution $ splitOn [""] $ lines contents

main = withFile "input/Day4.txt" ReadMode run

