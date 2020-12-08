import System.IO
import Data.List.Split
import qualified Data.Vector as V
import Debug.Trace (trace)

data Instruction = Instruction {
    operation :: String,
    argument :: Int
} deriving Show

data CodeLine = CodeLine {
    instruction :: Instruction,
    lineNum :: Int,
    executed :: Bool
} deriving Show

parse :: String -> Instruction
parse s = Instruction op (read normArg)
    where (op:arg:[]) = words s
          normArg = if startsWithPlus arg then (drop 1 arg) else arg
          startsWithPlus (ch:_) = ch == '+'

execute :: V.Vector CodeLine -> Int -> Int -> Int
execute ls i acc
    | ex = acc
    | otherwise = case inst of
        Instruction "nop" _ -> execute uls (i+1) acc
        Instruction "acc" v -> execute uls (i+1) (acc+v)
        Instruction "jmp" v -> execute uls (i+v) acc
    where CodeLine inst _ ex = ls V.! i
          uls = ls V.// [(i, CodeLine inst i True)]

toCodeLines :: [Instruction] -> V.Vector CodeLine
toCodeLines os = V.fromList $ map f (zip os [0..])
    where f (o,i) = CodeLine o i False

run h = do
    contents <- hGetContents h
    print $ execute (toCodeLines $ map parse $ lines contents) 0 0

main = withFile "input/Day8.txt" ReadMode run


