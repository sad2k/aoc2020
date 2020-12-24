import qualified Data.Set as Set
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

getNeighbourCoords :: (Int,Int) -> [(Int,Int)]
getNeighbourCoords (x,y) = [(x+1, y-2), (x-1, y-2), (x+1, y+2), (x-1, y+2), (x-2, y), (x+2, y)]

getNeighbourColours :: (Int,Int) -> Map.Map (Int,Int) Colour -> [Colour]
getNeighbourColours (x,y) m = concat $ map f (getNeighbourCoords (x,y))
    where f с = case (Map.lookup с m) of
                    Nothing -> []
                    Just colour -> [colour]

getExtraNeighbours :: Map.Map (Int,Int) Colour -> [(Int,Int)]
getExtraNeighbours m = Set.toList $ foldl f Set.empty (Map.keys m)
    where f s c = Set.union s (Set.fromList $ newNeighbours c)
          newNeighbours c = filter (\c -> not (Map.member c m)) $ getNeighbourCoords c

doFlips :: Map.Map (Int,Int) Colour -> Map.Map (Int,Int) Colour
doFlips m = Map.union flipExisting flipNeighbours
    where flipExisting = Map.mapWithKey doFlipExisting m
          doFlipExisting c col = let numOfBlacks = length $ filter (==Black) $ getNeighbourColours c m in
            case col of
                Black -> if numOfBlacks == 0 || numOfBlacks > 2 then White else Black
                White -> if numOfBlacks == 2 then Black else White
          flipNeighbours = Map.fromList $ filter (\x -> snd x == Black) $ map doFlipNeighbour (getExtraNeighbours m)
          doFlipNeighbour c = (c, doFlipExisting c White)

numOfBlacks :: Map.Map (Int,Int) Colour -> Int
numOfBlacks m = length $ filter (== Black) $ Map.elems m

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let coords = map (resolveCoordinate (0,0)) ls
    let m = resolveFlips coords
    let flipped = last $ take 101 $ iterate doFlips m
    print $ numOfBlacks flipped

main = withFile "input/Day24.txt" ReadMode run