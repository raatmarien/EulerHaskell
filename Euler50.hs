primeNums :: [Int]
primeNums = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

isPrime :: Int -> Bool
isPrime n = not $ or $ [ n `mod` x == 0
                       | x <- [2..(round (sqrt (fromIntegral n)))]]

consecutivePrimes :: Int -> Int -> Int
consecutivePrimes start width = sum $ map (primeNums !!)
                                [start..(start+width-1)]

main = print $ maximum $ take 10 [maximum $ filter isPrime
                                  $ takeWhile (<1000000)
                                  $ map (consecutivePrimes p)
                                  [0..]
                                  | p <- [1..]]








