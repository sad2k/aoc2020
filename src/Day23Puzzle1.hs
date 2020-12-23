import Data.List
import Data.List.Split
import Debug.Trace (trace)

maxNumber :: Int
maxNumber = 9

findDestination :: Int -> [Int] -> Int
findDestination 0 taken = findDestination maxNumber taken
findDestination n taken = if n `elem` taken then findDestination (n-1) taken else n

playRound :: [Int] -> [Int]
playRound (current:xs) = newList
    where threeCups = take 3 xs
          exceptThree = drop 3 xs
          destination = findDestination (current-1) threeCups
          newList = case (splitOn [destination] (exceptThree)) of
                        [beforeDestination,afterDestination] -> beforeDestination ++ [destination] ++ threeCups ++ afterDestination ++ [current]

putInOrderWithoutOne :: [Int] -> [Int]
putInOrderWithoutOne xs = case (splitOn [1] xs) of
    [beforeOne,afterOne] -> afterOne ++ beforeOne

main = do
    let input = "389125467"
    let parsedInput = map (\ch -> (read [ch]) :: Int) input
    let result = last $ take 101 $ iterate playRound parsedInput
    print $ map (head . show) (putInOrderWithoutOne result)