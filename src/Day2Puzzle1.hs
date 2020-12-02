
import Data.List.Split
import System.IO

data Rule = Rule { ruleChar :: Char,
    ruleMinCount :: Int,
    ruleMaxCount :: Int
} deriving Show

occurences ch s = length [x | x <- s, x == ch]

isValid password rule = o >= ruleMinCount rule && o <= ruleMaxCount  rule
    where o = occurences (ruleChar rule) password

parse str = (pwd, Rule ch imin imax)
    where (t1:pwd:[]) = splitOn ": " str
          (t2:(ch:[]):[]) = splitOn " " t1
          (smin:smax:[]) = splitOn "-" t2
          imin = read smin
          imax = read smax

run h = do
    contents <- hGetContents h
    print $ length $ filter id $ map (uncurry isValid . parse) $ lines contents

main = withFile "input/Day2.txt" ReadMode run

