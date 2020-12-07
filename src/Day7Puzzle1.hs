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

addRelationshipToMap :: Map.Map String [String] -> (String,Int) -> BagRule -> Map.Map String [String]
addRelationshipToMap m containedBag rule = Map.insertWith (++) (fst containedBag) [bagColour rule] m

addRuleToMap :: Map.Map String [String] -> BagRule -> Map.Map String [String]
addRuleToMap m r = foldl (\m p -> addRelationshipToMap m p r) m (mayContain r)

indexColours :: [BagRule] -> Map.Map String [String]
indexColours brs = foldl addRuleToMap Map.empty brs

traverseIndex :: Map.Map String [String] -> Set.Set String -> [String] -> Set.Set String
traverseIndex m s [] = s
traverseIndex m s (x:xs) = traverseIndex m (Set.union (Set.fromList containing) s) (containing ++ xs)
    where containing = Map.findWithDefault [] x m

findAllContainingBags :: Map.Map String [String] -> String -> Set.Set String
findAllContainingBags m b = traverseIndex m Set.empty [b]

run h = do
    contents <- hGetContents h
    let s = findAllContainingBags (indexColours $ map parse $ lines contents) "shiny gold"
    print $ s
    print $ length s

main = withFile "input/Day7.txt" ReadMode run