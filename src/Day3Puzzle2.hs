
import System.IO

type Column = Int

data Move = MoveDown Int | MoveRight Int

findSolution' :: [String] -> Column -> [Move] -> [Move] -> Int -> Int
findSolution' [] _ _ _ acc = acc
findSolution' (l:ls) curCol moves genMoves acc = case moves of
    [] -> let tree = if (l !! curCol) == '#' then 1 else 0
        in findSolution' (l:ls) curCol genMoves genMoves (acc+tree)
    ((MoveDown rows):ms) -> if rows == 0 then findSolution' (l:ls) curCol ms genMoves acc
        else findSolution' ls curCol ((MoveDown (rows-1)):ms) genMoves acc
    ((MoveRight cols):ms) -> findSolution' (l:ls) (curCol+cols) ms genMoves acc

diffGenMoves :: [[Move]]
diffGenMoves = [[MoveDown 1, MoveRight 1], [MoveDown 1, MoveRight 3], [MoveDown 1, MoveRight 5],
    [MoveDown 1, MoveRight 7], [MoveDown 2, MoveRight 1]]

findSolution :: [String] -> Int
findSolution ls = product $ map f diffGenMoves
    where f gm = findSolution' ls 0 gm gm 0

run h = do
    contents <- hGetContents h
    print $ findSolution $ map cycle $ lines contents

main = withFile "input/Day3.txt" ReadMode run

