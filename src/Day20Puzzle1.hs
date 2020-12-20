import System.IO
import Data.List
import Data.List.Split
import Debug.Trace (trace)

type Tile = (Int, [String])

parseTile :: [String] -> Tile
parseTile (x:xs) = (id, xs)
    where id = (read . init . head . tail) (splitOn " " x)

transposeTile :: Tile -> Tile
transposeTile (n, xs) = (n, transpose xs)

rotateRight :: Tile -> Tile
rotateRight (n, xs) = (n, (map reverse . transpose) xs)

rotateLeft :: Tile -> Tile
rotateLeft (n, xs) = (n, (reverse . transpose) xs)

flipHorizontally :: Tile -> Tile
flipHorizontally (n, xs) = (n, map reverse xs)

flipVertically :: Tile -> Tile
flipVertically (n, xs) = (n, reverse xs)

allowedTiles :: Int -> [Tile] -> [Tile] -> [Tile]
allowedTiles n acc xs = (filter isAllowed xs) ++ (filter isAllowed (map rotateRight xs)) ++ (filter isAllowed (map rotateLeft xs)) ++
                            (filter isAllowed (map flipHorizontally xs)) ++ (filter isAllowed (map flipVertically xs)) ++
                            (filter isAllowed (map (rotateRight . flipHorizontally) xs)) ++ (filter isAllowed (map (rotateRight . flipVertically) xs))
    where pos = (placedSoFar `mod` n, placedSoFar `div` n)
          placedSoFar = length acc
          isAllowed t = isMatchFromLeft t && isMatchFromTop t
          isMatchFromLeft t = if fst pos == 0 then True else (head . transpose . getLines) t == (last . transpose . getLines) (last acc)
          isMatchFromTop t = if snd pos == 0 then True else (head . getLines) t == (last . getLines . head . (drop $ (length acc)-n)) acc
          getLines t = snd t

exceptTile :: Tile -> Tile -> Bool
exceptTile t = \x -> fst x /= fst t

placeTiles :: Int -> [Tile] -> [Tile] -> [[Tile]]
placeTiles n acc [] = [acc]
placeTiles n acc xs = concat $ map f (allowedTiles n acc xs)
    where f t = placeTiles n (acc ++ [t]) (filter (exceptTile t) xs)

squareSize :: Int -> Int
squareSize n = round $ sqrt (fromIntegral n)

drawTile t = mapM_ print (snd t)

corners :: Int -> [Tile] -> [Integer]
corners n xs = map (\x -> toInteger $ fst $ xs !! (toIdx x)) coords
    where toIdx (x,y) = y * n + x
          coords = [(0,0),(0,n-1),(n-1,0),(n-1,n-1)]

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let tiles = map parseTile (splitOn [""] ls)
    let sqsz = squareSize (length tiles)
    print $ sqsz
    let res = placeTiles sqsz [] tiles
    let crn = map (\x -> corners sqsz x) res
    print crn
    print $ map product corners

main = withFile "input/Day20.txt" ReadMode run