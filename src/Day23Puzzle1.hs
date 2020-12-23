import Data.List
import Data.List.Split
import Debug.Trace (trace)

playRound :: [Int] -> [Int]
playRound (current:xs) = startingFromNewCurrent
    where threeCups = take 3 xs
          exceptThree = drop 3 xs
          destination = if (current-1) `elem` exceptThree then (current-1) else findDestination
          findDestination = if length allSmallerThanCurrent > 0 then maximum allSmallerThanCurrent else maximum allLargerThanCurrent
          allSmallerThanCurrent = filter (<current) exceptThree
          allLargerThanCurrent = filter (>current) exceptThree
          newList = case (splitOn [destination] (current:exceptThree)) of
                        [beforeDestination,afterDestination] -> beforeDestination ++ [destination] ++ threeCups ++ afterDestination
          startingFromNewCurrent = case (splitOn [current] newList) of
                        [beforeCurrent,afterCurrent] -> afterCurrent ++ beforeCurrent ++ [current]

putInOrderWithoutOne :: [Int] -> [Int]
putInOrderWithoutOne xs = case (splitOn [1] xs) of
    [beforeOne,afterOne] -> afterOne ++ beforeOne

main = do
    let input = "253149867"
    let parsedInput = map (\ch -> (read [ch]) :: Int) input
    let result = last $ take 101 $ iterate playRound parsedInput
    print $ map (head . show) (putInOrderWithoutOne result)