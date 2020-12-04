import System.IO
import Data.List.Split
import Data.Char
import qualified Data.Map as Map
import Debug.Trace (trace)

parsePassportLine :: String -> Map.Map String String -> Map.Map String String
parsePassportLine l m = foldr insertIntoMap m entries
    where entries = map (splitOn ":") (splitOn " " l)
          insertIntoMap (k:v:[]) map = Map.insert k v map

validations = [("byr", validateByr), ("iyr", validateIyr), ("eyr", validateEyr),
    ("hgt", validateHgt), ("hcl", validateHcl), ("ecl", validateEcl), ("pid", validatePid)]

validatePid v = length v == 9 && all isDigit v

validateEcl v = elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validateHcl v = length v == 7 && v !! 0 == '#' && checkChars (drop 1 v)
    where checkChars s = all (\c -> isDigit c || isLower c) s

validateByr v = i >= 1920 && i <= 2002
    where i = read v

validateIyr v = i >= 2010 && i <= 2020
    where i = read v

validateEyr v = i >= 2020 && i <= 2030
    where i = read v

validateHgt v = length v > 2 && checkHeight (read $ take (l-2) v) (drop (l-2) v)
    where l = length v
          checkHeight h "cm" = h >= 150 && h <= 193
          checkHeight h "in" = h >= 59 && h <= 76
          checkHeight h _ = False

checkMap :: Map.Map String String -> Bool
checkMap m = toBool $ fmap and $ sequence $ map checkVal validations
    where checkVal (k, f) = fmap f (Map.lookup k m)
          toBool (Just b) = b
          toBool Nothing = False

isValidPassport :: [String] -> Bool
isValidPassport ls = checkMap $ foldr parsePassportLine Map.empty ls

findSolution :: [[String]] -> Int
findSolution ls = (length . filter id . map isValidPassport) ls

run h = do
    contents <- hGetContents h
    print $ findSolution $ splitOn [""] $ lines contents

main = withFile "input/Day4.txt" ReadMode run

