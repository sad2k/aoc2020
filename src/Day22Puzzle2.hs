import Data.List.Split
import Data.Sort
import qualified Data.Set as Set
import System.IO

data GameState = MkGameState {
    player1 :: [Int],
    player2 :: [Int],
    previous :: Set.Set ([Int],[Int]),
    winner :: Maybe Int
} deriving Show

getCards :: [String] -> [[Int]]
getCards xs = map (map read . drop 1) (splitOn [""] xs)

playRound :: GameState -> GameState
playRound st = case roundResult of
                    1 -> MkGameState ((fst remainingCards) ++ [fst drawnCards] ++ [snd drawnCards]) (snd remainingCards) updatedSet Nothing
                    2 -> MkGameState (fst remainingCards) (snd remainingCards ++ [snd drawnCards] ++ [fst drawnCards]) updatedSet Nothing
                    _ -> error "this should never happen!"
    where drawnCards = ((head $ player1 st), (head $ player2 st))
          remainingCards = ((tail $ player1 st), (tail $ player2 st))
          roundResult = if needToRecurse then playRecursive else playNormal
          playNormal = if (fst drawnCards) > (snd drawnCards) then 1 else 2
          playRecursive = case (winner $ playGame $ MkGameState (take (fst drawnCards) $ fst remainingCards) (take (snd drawnCards) $ snd remainingCards) Set.empty Nothing) of
            Just winner -> winner
            Nothing -> error "this should never happen!"
          needToRecurse = (fst drawnCards) <= (length $ fst remainingCards) && (snd drawnCards) <= (length $ snd remainingCards)
          updatedSet = ((player1 st, player2 st) `Set.insert` previous st)

playGame :: GameState -> GameState
playGame st
    | (player1 st, player2 st) `Set.member` (previous st) = MkGameState (player1 st) (player2 st) (previous st) (Just 1)
    | null (player1 st) = MkGameState (player1 st) (player2 st) (previous st) (Just 2)
    | null (player2 st) = MkGameState (player1 st) (player2 st) (previous st) (Just 1)
    | otherwise = playGame (playRound st)

run h = do
    contents <- hGetContents h
    let ls = lines contents
    let [p1, p2] = getCards ls
    let initialState = MkGameState p1 p2 Set.empty Nothing
    let result = playGame initialState
    let winningCards = if null (player1 result) then player2 result else player1 result
    print winningCards
    print $ sum $ map (\p -> fst p * snd p) $ zip winningCards [length winningCards,((length winningCards)-1)..1]

main = withFile "input/Day22.txt" ReadMode run