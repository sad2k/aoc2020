import System.IO
import Data.List.Split
import qualified Data.Map as Map
import Data.Bits
import Debug.Trace (trace)

data Entry = Mask [(Int,Int)] [Int] | Mem Int Int deriving Show

parseMask :: String -> Entry
parseMask s = Mask realBits floatingBits
    where realBits = map (\x -> (fst x, read [(snd x)])) $ filter (\x -> (snd x) /= 'X') bits
          floatingBits = map (\x -> fst x) $ filter (\x -> (snd x) == 'X') bits
          bits = (zip [0..] (reverse s))

generateFloating :: [Int] -> [Int] -> [Int]
generateFloating vals [] = vals
generateFloating vals (bit:bits) = generateFloating (concat $ map f vals) bits
    where f x = [x `setBit` bit, x `clearBit` bit]

parse :: [String] -> [Entry]
parse xs = map f xs
    where spl x = splitOn " = " x
          f x = case (spl x) of
            ["mask",v] -> parseMask v
            [k,v] -> Mem ((read . init . drop 4) k) (read v)

applyRealMask :: [(Int,Int)] -> Int -> Int
applyRealMask mask val = foldl f val mask
    where f cur (bitaddr,bitval) = if bitval == 1 then cur `setBit` bitaddr else cur

applyMask :: Entry -> Int -> [Int]
applyMask (Mask realMask floatMask) val = generateFloating [applyRealMask realMask val] floatMask

findSolution :: [Entry] -> Entry -> Map.Map Int Int -> Map.Map Int Int
findSolution [] mask m = m
findSolution (x:xs) mask m = case x of
    Mask newRealMask newFloatMask -> findSolution xs (Mask newRealMask newFloatMask) m
    Mem addr val -> findSolution xs mask (foldl (\acc x -> Map.insert x val acc) m (allAddrs addr))
        where allAddrs addr = applyMask mask addr

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let parsed = parse ls
    let sol = findSolution parsed (Mask [] []) Map.empty
    print $ sol
    let s = foldl (+) 0 sol
    print s

main = withFile "input/Day14.txt" ReadMode run
