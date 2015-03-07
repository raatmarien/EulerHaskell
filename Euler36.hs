import Data.List

toReverseBinaryList :: Int -> [Int]
toReverseBinaryList 0 = []
toReverseBinaryList 1 = [1]
toReverseBinaryList n = (n `mod` 2):(toReverseBinaryList $ n `div` 2)

isPalindromic :: Eq a => [a] -> Bool
isPalindromic xs = xs == (reverse xs)

main = print $ sum $ filter (\n -> isPalindromic (show n) && (isPalindromic (toReverseBinaryList n))) [1..999999]
