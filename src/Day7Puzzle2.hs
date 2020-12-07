import System.IO
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

data BagRule = MkBagRule {
    bagColour :: String,
    mayContain :: [(String, Int)]
} deriving Show

startsWithDigit [] = False
startsWithDigit (x:xs) = isDigit x

parseContentBag :: [String] -> (String, Int)
parseContentBag (x:xs) = (intercalate " " xs, read x)

parseContents l = map parseContentBag l

parse s = MkBagRule bagColour (parseContents contents)
    where filterChar ch = isAlpha ch || isDigit ch || isSpace ch
          w = words (filter filterChar s)
          bagColour = intercalate " " $ takeWhile (/= "bags") w
          contents = filter (\l -> length l > 0) $ splitWhen (\s -> isPrefixOf "bag" s) $ dropWhile (not . startsWithDigit) w

addRelationshipToMap :: Map.Map String [(String,Int)] -> (String,Int) -> BagRule -> Map.Map String [(String,Int)]
addRelationshipToMap m containedBag rule = Map.insertWith (++) (bagColour rule) [containedBag] m

addRuleToMap :: Map.Map String [(String,Int)] -> BagRule -> Map.Map String [(String,Int)]
addRuleToMap m r = foldl (\m p -> addRelationshipToMap m p r) m (mayContain r)

indexColours :: [BagRule] -> Map.Map String [(String,Int)]
indexColours brs = foldl addRuleToMap Map.empty brs

findNumContainedBags :: Map.Map String [(String,Int)] -> String -> Int
findNumContainedBags m x = sum (map f contained)
    where contained = Map.findWithDefault [] x m
          f (b,i) = i + (i * (findNumContainedBags m b))

run h = do
    contents <- hGetContents h
    print $ findNumContainedBags (indexColours $ map parse $ lines contents) "shiny gold"

main = withFile "input/Day7.txt" ReadMode run