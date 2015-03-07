primeNums :: [Int]
primeNums = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
    
isPrime :: Int -> Bool
isPrime n = not $ or $ [ n `mod` x == 0
                       | x <- [2..(round (sqrt (fromIntegral n)))]]

oddCompositeNums :: [Int]
oddCompositeNums = filter (not . isPrime) $ filter odd [1..] 

squareNums :: [Int]
squareNums = map (\n -> n * n) [1..]

followsGoldbachConjecture :: Int -> Bool
followsGoldbachConjecture n = or [ n == (x + (2 * y))
                                 | x <- takeWhile (<(n)) primeNums
                                 , y <- takeWhile (<(n `div` 2)) squareNums]

main :: IO ()
main = print $ head $ filter (not . followsGoldbachConjecture) oddCompositeNums
