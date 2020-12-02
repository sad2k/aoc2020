
import Data.List.Split
import System.IO

data Rule = Rule { ruleChar :: Char,
    firstPos :: Int,
    secondPos :: Int
} deriving Show

isValid password rule = ((password !! fp) == ch) /= ((password !! sp) == ch)
    where ch = ruleChar rule
          fp = (firstPos rule) - 1
          sp = (secondPos rule) - 1

parse str = (pwd, Rule ch f s)
    where (t1:pwd:[]) = splitOn ": " str
          (t2:(ch:[]):[]) = splitOn " " t1
          (sf:ss:[]) = splitOn "-" t2
          f = read sf
          s = read ss

run h = do
    contents <- hGetContents h
    print $ length $ filter id $ map (uncurry isValid . parse) $ lines contents

main = withFile "input/Day2.txt" ReadMode run

