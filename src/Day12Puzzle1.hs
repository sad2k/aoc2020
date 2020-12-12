import Prelude hiding (Left, Right)
import System.IO
import Debug.Trace (trace)

data Action = North | South | East | West | Left | Right | Forward deriving (Show, Eq)

parse :: [String] -> [(Action,Int)]
parse xs = map f xs
    where f (x:xs) = (parseAction x, read xs)
          parseAction 'N' = North
          parseAction 'S' = South
          parseAction 'E' = East
          parseAction 'W' = West
          parseAction 'L' = Left
          parseAction 'R' = Right
          parseAction 'F' = Forward

changeBearing :: Int -> Action -> Int -> Int
changeBearing bearing act val = (bearing + chg) `mod` 360
    where chg = val * sgn
          sgn = if act == Left then -1 else 1

changeCoord (x,y) 0 val = (x,y+val)
changeCoord (x,y) (-90) val = (x-val,y)
changeCoord (x,y) (-180) val = (x,y-val)
changeCoord (x,y) (-270) val = (x+val,y)
changeCoord (x,y) 270 val = (x-val,y)
changeCoord (x,y) 180 val = (x,y-val)
changeCoord (x,y) 90 val = (x+val,y)

navigate :: [(Action,Int)] -> (Int,Int) -> Int -> (Int,Int)
navigate [] cur _ = cur
navigate ((act,val):xs) (x,y) bearing = case act of
    North -> navigate xs (x,y+val) bearing
    South -> navigate xs (x,y-val) bearing
    East -> navigate xs (x+val,y) bearing
    West -> navigate xs (x-val,y) bearing
    Left -> navigate xs (x,y) (changeBearing bearing act val)
    Right -> navigate xs (x,y) (changeBearing bearing act val)
    Forward -> navigate xs (changeCoord (x,y) bearing val) bearing

run h = do
    contents <- hGetContents h
    let parsed = parse (lines contents)
    let res = navigate parsed (0,0) 90
    print $ ((abs . fst) res) + ((abs . snd) res)

main = withFile "input/Day12.txt" ReadMode run