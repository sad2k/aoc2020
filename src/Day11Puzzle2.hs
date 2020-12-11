import System.IO
import Data.List
import Debug.Trace (trace)
import qualified Data.Vector as V

convert :: [String] -> [([(Char,Int)],Int)]
convert xs = zip (map f xs) [0..]
    where f s = zip s [0..]

isOccupied s = f $ filter (/= '.') (map fst s)
    where f [] = 0
          f ('#':_) = 1
          f _ = 0

generateDiagonal :: (Int,Int) -> (Int -> Int) -> (Int -> Int) -> Int -> Int -> [(Int,Int)]
generateDiagonal (row,col) rowFun colFun maxRow maxCol = if (newRow >= 0 && newRow <= maxRow && newCol >= 0 && newCol <= maxCol)
    then (newRow,newCol):(generateDiagonal (newRow,newCol) rowFun colFun maxRow maxCol)
    else []
    where (newRow,newCol) = (rowFun row, colFun col)

getElements :: [([(Char,Int)],Int)] -> [(Int,Int)] -> [(Char,Int)]
getElements xs coords = map f coords
    where f (r,c) = (fst (xs !! r)) !! c

transform :: [([(Char,Int)],Int)] -> [String]
transform xs = map transformRow xs
    where transformRow (s,rowNum) = map (\p -> transformSeat p rowNum)  s
          transformSeat (ch,c) r = case ch of
            'L' -> if occupied (r,c) == 0 then '#' else 'L'
            '#' -> if occupied (r,c) >= 5 then 'L' else '#'
            someCh -> someCh
          occupied (r,c) = occLeft (r,c) + occRight(r,c) + occUp(r,c) + occDown(r,c) + occLeftUp(r,c) + occLeftDown(r,c) + occRightUp(r,c) + occRightDown(r,c)
          occLeft (r,c) = isOccupied $ (reverse . take c) (fst (xs !! r))
          occRight (r,c) = isOccupied $ drop (c+1) (fst (xs !! r))
          occUp (r,c) = isOccupied $ (if r == 0 then [] else ((reverse . take r) (getCol c)))
          occDown (r,c) = isOccupied $ (if r == maxRow then [] else (drop (r+1) (getCol c)))
          -- this is horrible - needs to be rewritten to use something (previousLines, nextLine, previousChars, nextChars etc instead of indexing into a list)
          occLeftUp (r,c) = isOccupied $ getElements xs (generateDiagonal (r,c) (\x -> x-1) (\x -> x-1) maxRow maxCol)
          occLeftDown (r,c) = isOccupied $ getElements xs (generateDiagonal (r,c) (\x -> x+1) (\x -> x-1) maxRow maxCol)
          occRightUp (r,c) = isOccupied $ getElements xs (generateDiagonal (r,c) (\x -> x-1) (\x -> x+1) maxRow maxCol)
          occRightDown (r,c) = isOccupied $ getElements xs (generateDiagonal (r,c) (\x -> x+1) (\x -> x+1) maxRow maxCol)
          maxRow = (length xs) - 1
          maxCol = (length ((fst . head) xs) - 1)
          getCol c = map (\s -> (fst s) !! c) xs

findSolution :: [String] -> [[String]]
findSolution xs = if (xs == tr) then [tr] else tr:(findSolution tr)
    where tr = (transform . convert) xs

run h = do
    contents <- hGetContents h
    let l = lines contents

    let sol = findSolution (lines contents)
--     mapM print sol
    print $ (length . filter (=='#') . concat . last) sol

main = withFile "input/Day11.txt" ReadMode run