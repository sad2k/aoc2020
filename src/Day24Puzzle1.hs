import qualified Data.Map as Map
import System.IO

data Colour = Black | White deriving (Show, Eq)

resolveCoordinate :: (Int,Int) -> String -> (Int,Int)
resolveCoordinate (x,y) [] = (x,y)
resolveCoordinate (x,y) ('s':('e':xs)) = resolveCoordinate (x+1,y-2) xs
resolveCoordinate (x,y) ('s':('w':xs)) = resolveCoordinate (x-1,y-2) xs
resolveCoordinate (x,y) ('n':('e':xs)) = resolveCoordinate (x+1,y+2) xs
resolveCoordinate (x,y) ('n':('w':xs)) = resolveCoordinate (x-1,y+2) xs
resolveCoordinate (x,y) ('w':xs) = resolveCoordinate (x-2,y) xs
resolveCoordinate (x,y) ('e':xs) = resolveCoordinate (x+2,y) xs

resolveFlips :: [(Int,Int)] -> Map.Map (Int,Int) Colour
resolveFlips coords = foldl f Map.empty coords
    where f m c = case (Map.lookup c m) of
                    Nothing -> Map.insert c Black m
                    Just colour -> Map.insert c (flipColour colour) m
          flipColour Black = White
          flipColour White = Black

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let coords = map (resolveCoordinate (0,0)) ls
    let m = resolveFlips coords
    print $ length $ filter (== Black) $ Map.elems m

main = withFile "input/Day24.txt" ReadMode run