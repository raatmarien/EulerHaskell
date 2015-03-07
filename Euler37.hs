isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = not $ or $ [ n `mod` x == 0
                       | x <- [2..(round (sqrt (fromIntegral n)))]]

isTruncablePrime :: Int -> Bool
isTruncablePrime n = isTruncablePrime' $ show n
  where
    isTruncablePrime' [x] = isPrime (read [x] :: Int) 
    isTruncablePrime' (x:xs) = if isPrime x'
                               then isTruncablePrime' xs
                               else False
      where
        x' = read (x:xs) :: Int

isReverseTruncablePrime :: Int -> Bool
isReverseTruncablePrime n = isReverseTruncablePrime' $ reverse $ show n
  where
    isReverseTruncablePrime' [x] = isPrime (read [x] :: Int)
    isReverseTruncablePrime' (x:xs) = if isPrime x'
                                      then isReverseTruncablePrime' xs
                                      else False
      where
        x' = read $ reverse (x:xs) :: Int

isMultiDirectionalTruncablePrime :: Int -> Bool
isMultiDirectionalTruncablePrime n = (isTruncablePrime n) && (isReverseTruncablePrime n)

main = print $ sum tList
  where
    tList = take 11 $ filter isMultiDirectionalTruncablePrime [11..]
