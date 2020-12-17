import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Int,Int,Int,Int)

data State = Active | Inactive deriving (Show, Eq)

getState :: Char -> State
getState '.' = Inactive
getState '#' = Active

parse' :: [String] -> Map.Map Coord State -> Int -> Map.Map Coord State
parse' [] m y = m
parse' (x:xs) m y = parse' xs newM (y+1)
    where newM = foldl (\map p -> Map.insert ((snd p), y, 0, 0) (getState $ fst p) map) m (zip x [0..])

parse :: [String] -> Map.Map Coord State
parse xs = parse' xs Map.empty 0

neighbours :: Coord -> [Coord]
neighbours (x,y,z,w) = [(x+x', y+y', z+z', w+w') | x' <- [-1,0,1], y' <- [-1,0,1], z' <- [-1,0,1], w' <- [-1,0,1], (abs x')+(abs y')+(abs z')+(abs w') > 0]

newNeighboursForCoord :: Map.Map Coord State -> Coord -> Set.Set Coord
newNeighboursForCoord m c = Set.fromList $ filter flt (neighbours c)
    where flt c = not (Map.member c m)

newNeighbours :: Map.Map Coord State -> Set.Set Coord
newNeighbours m = foldl f Set.empty (Map.keys m)
    where f s c = s `Set.union` (newNeighboursForCoord m c)

resolveStateChange :: State -> [State] -> State
resolveStateChange s ns = case s of
            Active -> if (activeNeighbours == 2) || (activeNeighbours == 3) then Active else Inactive
            Inactive -> if activeNeighbours == 3 then Active else Inactive
    where activeNeighbours = length $ filter (== Active) ns

updateState :: Map.Map Coord State -> Coord -> State -> State
updateState m c curState = resolveStateChange curState (foldl f [] (neighbours c))
    where f l coord = case (Map.lookup coord m) of
            Just st -> st:l
            Nothing -> l

runCycle :: Map.Map Coord State -> Map.Map Coord State
runCycle m = Map.union updateExisting addNew
    where updateExisting = Map.mapWithKey (\k v -> updateState m k v) m
          addNew = Map.fromList $ filter (\p -> (snd p) == Active) $ map (\n -> (n, (updateState m n Inactive))) (Set.toList $ newNeighbours m)

numActive :: Map.Map Coord State -> Int
numActive m = length $ filter (== Active) $ Map.elems m

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let initial = parse ls
    print initial
    let after6 = (iterate runCycle initial) !! 6
    print $ after6
    print $ numActive after6

main = withFile "input/Day17.txt" ReadMode run

