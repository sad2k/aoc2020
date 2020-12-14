import System.IO
import Data.List.Split
import qualified Data.Map as Map
import Data.Bits
import Debug.Trace (trace)

data Entry = Mask [(Int,Int)] | Mem Int Int deriving Show

parseMask :: String -> [(Int,Int)]
parseMask s = map (\x -> (fst x, read ([snd x]))) $ filter (\x -> (snd x) /= 'X') (zip [0..] (reverse s))

parse :: [String] -> [Entry]
parse xs = map f xs
    where spl x = splitOn " = " x
          f x = case (spl x) of
            ["mask",v] -> Mask (parseMask v)
            [k,v] -> Mem ((read . init . drop 4) k) (read v)

applyMask :: [(Int,Int)] -> Int -> Int
applyMask mask val = foldl f val mask
    where f cur (bitaddr,bitval) = if bitval == 1 then cur `setBit` bitaddr else cur `clearBit` bitaddr

findSolution :: [Entry] -> [(Int,Int)] -> Map.Map Int Int -> Map.Map Int Int
findSolution [] mask m = m
findSolution (x:xs) mask m = case x of
    Mask newMask -> findSolution xs newMask m
    Mem addr val -> findSolution xs mask (Map.insert addr (applyMask mask val) m)

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let parsed = parse ls
    let sol = findSolution parsed [] Map.empty
    print $ sol
    let s = foldl (+) 0 sol
    print s

main = withFile "input/Day14.txt" ReadMode run
