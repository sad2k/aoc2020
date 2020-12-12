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

rotate (x,y) Right 90 = (y,-x)
rotate (x,y) _ 180 = (-x,-y)
rotate (x,y) Right 270 = (-y,x)
rotate (x,y) Left 270 = rotate (x,y) Right 90
rotate (x,y) Left 90 = rotate (x,y) Right 270

navigate :: [(Action,Int)] -> (Int,Int) -> (Int,Int) -> (Int,Int)
navigate [] cur _ = cur
navigate ((act,val):xs) (x,y) (wx,wy) = case act of
    North -> navigate xs (x,y) (wx,wy+val)
    South -> navigate xs (x,y) (wx,wy-val)
    East -> navigate xs (x,y) (wx+val,wy)
    West -> navigate xs (x,y) (wx-val,wy)
    Left -> navigate xs (x,y) (rotate (wx,wy) act val)
    Right -> navigate xs (x,y) (rotate (wx,wy) act val)
    Forward -> navigate xs (x+wx*val,y+wy*val) (wx,wy)

run h = do
    contents <- hGetContents h
    let parsed = parse (lines contents)
    let res = navigate parsed (0,0) (10,1)
    print $ ((abs . fst) res) + ((abs . snd) res)

main = withFile "input/Day12.txt" ReadMode run