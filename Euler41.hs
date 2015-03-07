import Data.List
import Data.Char

isPrime :: Int -> Bool
isPrime n = not $ or $ [ n `mod` x == 0
                       | x <- [2..(round (sqrt (fromIntegral n)))]]

primePandigitals :: Int -> [Int]
primePandigitals n = filter isPrime $ map read $ permutations ['1'..(intToDigit n)]

main = print $ foldl max 0 $ concat $ map primePandigitals [1..9]
