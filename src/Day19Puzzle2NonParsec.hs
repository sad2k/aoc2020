import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List.Split
import Data.Either
import qualified Data.Map as Map
import Debug.Trace (trace)

data Rule = CharRule Char | OrRule Rule Rule | SeqRule [Rule] deriving Show

indexRules :: [String] -> Map.Map Int String
indexRules xs = Map.fromList $ map parseRule xs
    where parseRule x = case (splitOn ": " x) of
                            [ruleNum, rule] -> (read ruleNum, rule)

parseRule :: Map.Map Int String -> Int -> Rule
parseRule m n = case (Map.lookup n m) of
                    Nothing -> error $ "not found: " ++ (show n)
                    Just s -> doParse s
    where doParse s = case s of
                    ['"', ch, '"'] -> CharRule ch
                    _ -> if '|' `elem` s then orRule $ splitOn " | " s
                         else parseSeqRule s
          parseSeqRule s = SeqRule $ map (parseRule m . read) (splitOn " " s)
          orRule [r1,r2] = OrRule (parseSeqRule r1) (parseSeqRule r2)

isValid' :: String -> Rule -> (String -> Bool) -> Bool
isValid' [] (CharRule _) restChecker = False
isValid' (x:xs) (CharRule ch) restChecker = if x == ch then restChecker xs else False
isValid' xs (SeqRule []) restChecker = restChecker xs
isValid' xs (SeqRule (r:rs)) restChecker = isValid' xs r (\s -> isValid' s (SeqRule rs) restChecker)
isValid' xs (OrRule r1 r2) restChecker = isValid' xs r1 restChecker || isValid' xs r2 restChecker

isValid :: String -> Rule -> Bool
isValid xs r = isValid' xs r null

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let [rules, input] = splitOn [""] ls
    let rulesMap = indexRules rules
    let updatedRulesMap = Map.insert 8 "42 | 42 8" $ Map.insert 11 "42 31 | 42 11 31" $ rulesMap
    let rule = parseRule updatedRulesMap 0
    print $ length $ filter (id) $ map (\x -> isValid x rule) input

main = withFile "input/Day19.txt" ReadMode run