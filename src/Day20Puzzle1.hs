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

sumOfCorners :: Int -> [Tile] -> Integer
sumOfCorners n xs = product $ map (\x -> toInteger $ fst $ xs !! (toIdx x)) coords
    where toIdx (x,y) = y * n + x
          coords = [(0,0),(0,n-1),(n-1,0),(n-1,n-1)]

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let tiles = map parseTile (splitOn [""] ls)
--     mapM_ print tiles
    let sqsz = squareSize (length tiles)
    print $ sqsz
--     print sqsz
    let res = placeTiles sqsz [] tiles
    print $ map (\x -> sumOfCorners sqsz x) res

--     print $ allowedTiles sqsz [tiles !! 1] (tiles)
--     mapM_ print $ (transpose . snd) (tiles !! 1)
--     print "--"
--     mapM_ print $ (snd . head) tiles
--     print "--"
--     mapM_ print $ (snd . last) tiles
   -- testing
--     let tile1 = flipVertically $ tiles !! 1
--     let except1 = filter (exceptTile tile1) tiles
--     let tile2 = head $ allowedTiles sqsz [tile1] except1
--     let except2 = filter (exceptTile tile2) except1
--     let tile3 = head $ allowedTiles sqsz [tile1, tile2] except2
--     let except3 = filter (exceptTile tile3) except2
--     let tile4 = head $ allowedTiles sqsz [tile1, tile2, tile3] except3
--     let except4 = filter (exceptTile tile4) except3
--     let tile5 = head $ allowedTiles sqsz [tile1, tile2, tile3, tile4] except4
--     let except5 = filter (exceptTile tile5) except4
--     let tile6 = head $ allowedTiles sqsz [tile1, tile2, tile3, tile4, tile5] except5
--     let except6 = filter (exceptTile tile6) except5
--     let tile7 = head $ allowedTiles sqsz [tile1, tile2, tile3, tile4, tile5, tile6] except6
--     let except7 = filter (exceptTile tile7) except6
--     let tile8 = head $ allowedTiles sqsz [tile1, tile2, tile3, tile4, tile5, tile6, tile7] except7
--     let except8 = filter (exceptTile tile8) except7
--     print $ allowedTiles sqsz [tile1, tile2, tile3, tile4, tile5, tile6, tile7, tile8] except8
--     let testTile = (0, ["123","456","789"])
--     drawTile $ (rotateLeft . flipVertically) testTile
--     print "---"
--     drawTile $ (rotateLeft . flipHorizontally) testTile


main = withFile "input/Day20.txt" ReadMode run