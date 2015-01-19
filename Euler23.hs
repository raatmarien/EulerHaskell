divisors :: Int -> [Int]
divisors n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]

isAbundant :: Int -> Bool
isAbundant n = (sum $ divisors n) > n

abundantNums :: [Int]
abundantNums = filter (isAbundant) [1..28123]

isSumOfTwo :: Int -> [Int] -> Bool
isSumOfTwo n xs = or sets
  where
    xs' = takeWhile (<=n) xs
    sets = [x + y == n | x <- xs', y <- xs']

isSumOfAbundantNums :: Int -> Bool
isSumOfAbundantNums n = isSumOfTwo n abundantNums
    
-- main :: IO ()
-- main = print $ sum $ filter (not . isSumOfAbundantNums) [1..28123]
       
