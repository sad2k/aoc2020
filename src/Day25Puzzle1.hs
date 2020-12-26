import System.IO

findLoopSize :: Integer -> Integer -> Integer -> Integer -> Integer
findLoopSize value subject loop target = if value'' == target
    then loop
    else findLoopSize value'' subject (loop+1) target
    where value' = value * subject
          value'' = value' `mod` 20201227

calculateEncryptionKey :: Integer -> Integer -> Integer -> Integer
calculateEncryptionKey value subject loop = if loop == 0 then value else calculateEncryptionKey value'' subject (loop-1)
    where value' = value * subject
          value'' = value' `mod` 20201227

main = do
    let cardPublicKey = 10212254
    let doorPublicKey = 12577395
    let cardLoopSize = findLoopSize 1 7 1 cardPublicKey
    let doorLoopSize = findLoopSize 1 7 1 doorPublicKey
    print $ calculateEncryptionKey 1 cardPublicKey doorLoopSize
