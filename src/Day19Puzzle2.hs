import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List.Split
import qualified Data.Map as Map
import Debug.Trace (trace)

parseCh :: Char -> Parser String
parseCh ch = do
    v <- char ch
    return [ch]

indexRules :: [String] -> Map.Map Int String
indexRules xs = Map.fromList $ map parseRule xs
    where parseRule x = case (splitOn ": " x) of
                            [ruleNum, rule] -> (read ruleNum, rule)

getComplexParser :: Map.Map Int String -> String -> Parser String
getComplexParser m r = if '|' `elem` r
    then choice $ map (\x -> try $ getComplexParser m x) (splitOn " | " r)
    else do
        let parsers = map (\x -> getParser m (read x)) (splitOn " " r)
        res <- sequence parsers
        return $ concat res

getParser :: Map.Map Int String -> Int -> Parser String
getParser rules n = p
    where p = case (Map.lookup n rules) of
                Nothing -> error $ "rule " ++ (show n) ++ " isn't defined"
                Just r -> case r of
                    ['"', ch, '"'] -> parseCh ch
                    _ -> getComplexParser rules r

isValid :: String -> Parser String -> Bool
isValid s parser = case (parse (parser <* eof) "" s) of
                  Left err -> False
                  Right e -> True

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let [rules, input] = splitOn [""] ls
    let rulesMap = indexRules rules
--     let parser = getParser rulesMap 0
    let parser = getParser (Map.insert 8 "42 | 42 8" $ Map.insert 11 "42 31 | 42 11 31" $ rulesMap) 0
    print $ length $ filter (id) $ map (\x -> isValid x parser) input

main = withFile "input/Day19.txt" ReadMode run