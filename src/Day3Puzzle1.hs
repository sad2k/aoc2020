
import System.IO

type Column = Int

data Move = MoveDown Int | MoveRight Int

genMoves :: [Move]
genMoves = [MoveDown 1, MoveRight 3]

findSolution' :: [String] -> Column -> [Move] -> Int -> Int
findSolution' [] _ _ acc = acc
findSolution' (l:ls) curCol moves acc = case moves of
    [] -> let tree = if (l !! curCol) == '#' then 1 else 0
        in findSolution' (l:ls) curCol genMoves (acc+tree)
    ((MoveDown rows):ms) -> if rows == 0 then findSolution' (l:ls) curCol ms acc
        else findSolution' ls curCol ((MoveDown (rows-1)):ms) acc
    ((MoveRight cols):ms) -> findSolution' (l:ls) (curCol+cols) ms acc

findSolution :: [String] -> Int
findSolution ls = findSolution' ls 0 genMoves 0

run h = do
    contents <- hGetContents h
    print $ findSolution $ map cycle $ lines contents

main = withFile "input/Day3.txt" ReadMode run

