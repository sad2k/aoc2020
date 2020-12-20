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

allRotationsAndFlips :: [Tile] -> [Tile]
allRotationsAndFlips xs = concat $ map applyMask masks
    where masks = [(False,False,False),(True,False,False),(False,True,False),(False,False,True),
                   (True,True,False),(False,True,True),(True,False,True),(True,True,True)] -- should be done in a monadic way somehow?
          applyMask (isTranspose,isFlipVertically,isFlipHorizontally) = let fun1 = if isTranspose then transpose else id
                                                                            fun2 = if isFlipVertically then reverse else id
                                                                            fun3 = if isFlipHorizontally then (map reverse) else id
                                                                        in map (\t -> (fst t, (fun1 . fun2 . fun3) (snd t))) xs

allowedTiles :: Int -> [Tile] -> [Tile] -> [Tile]
allowedTiles n acc xs = res
    where res = filter isAllowed (allRotationsAndFlips xs)
          pos = (placedSoFar `mod` n, placedSoFar `div` n)
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

drawTile t = do
    putStrLn "Tile:"
    mapM_ print (snd t)

corners :: Int -> [Tile] -> [Integer]
corners n xs = map (\x -> toInteger $ fst $ xs !! (toIdx x)) coords
    where toIdx (x,y) = y * n + x
          coords = [(0,0),(0,n-1),(n-1,0),(n-1,n-1)]

removeCorners :: Tile -> Tile
removeCorners t = (fst t, rm $ snd t)
    where rm xs = map (init . tail) ((init . tail) xs)

groupTiles :: Int -> [Tile] -> [[Tile]]
groupTiles n xs = chunksOf n xs

combineHorizontally :: Tile -> Tile -> Tile
combineHorizontally t1 t2 = (0, cmb)
    where cmb = map (\p -> fst p ++ snd p) (zip (snd t1) (snd t2))

combineVertically :: Tile -> Tile -> Tile
combineVertically t1 t2 = (0, cmb)
    where cmb = (snd t1) ++ (snd t2)

combine :: [[Tile]] -> Tile
combine xs = foldl1 combineVertically (map f xs)
    where f xs = foldl1 combineHorizontally xs

buildMonsterMask :: [String] -> [[Int]]
buildMonsterMask xs = map f xs
    where f s = concat $ map (\p -> if fst p == '#' then [snd p] else []) (zip s [0..])

tryReplaceFromOffset :: [String] -> [[Int]] -> Int -> [String]
tryReplaceFromOffset xs coords offset = if offsetsMatch then replacedMonster else xs
    where offsetsMatch = all offsetsMatchForRow (zip xs coords)
          offsetsMatchForRow (s,c) = all (\x -> s !! (x+offset) == '#') c
          replacedMonster = map replacedMonsterForRow (zip xs coords)
          replacedMonsterForRow (s,c) = map (\p -> if ((snd p)-offset) `elem` c then 'O' else fst p) (zip s [0..])

replaceMonster' :: [String] -> [[Int]] -> [String]
replaceMonster' xs coords
    | length xs /= length coords = error "incorrect dim!"
    | otherwise = foldl (\curxs offset -> tryReplaceFromOffset curxs coords offset) xs possibleColOffsets
    where maskLength = maximum (map last coords)
          rowLength = length (head xs)
          possibleColOffsets = [0..(rowLength-maskLength-1)]

replaceMonster :: [String] -> [String] -> [[Int]] -> [String]
replaceMonster doneT t coords = if length t < length coords
                                then (doneT ++ t)
                                else replaceMonster (doneT ++ (take 1 processedHead)) ((drop 1 processedHead) ++ (drop (length processedHead) t)) coords
    where processedHead = replaceMonster' (take (length coords) t) coords

replaceMonsterTile :: Tile -> [[Int]] -> Tile
replaceMonsterTile t coords = (0, replaceMonster [] (snd t) coords)

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let tiles = map parseTile (splitOn [""] ls)
    let sqsz = squareSize (length tiles)
    let res = placeTiles sqsz [] tiles
    let singleRes = head $ res
    let grouped = (groupTiles sqsz (map removeCorners singleRes))
    let combined = combine grouped
    let seaMonster = ["                  # ",
                      "#    ##    ##    ###",
                      " #  #  #  #  #  #   "]
    let seaMonsterMask = buildMonsterMask seaMonster
    let testTiles = allRotationsAndFlips [combined]
    let replacedTestTiles = map (\t -> replaceMonsterTile t seaMonsterMask) testTiles
    let succeededReplacement = head $ filter (\t -> any (=='O') ((concat . snd) t)) replacedTestTiles
    drawTile $ succeededReplacement
    print $ (length . filter (=='#') . concat . snd) succeededReplacement

main = withFile "input/Day20.txt" ReadMode run