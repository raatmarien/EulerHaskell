import Data.List

isOneToNinePandigital :: String -> Bool
isOneToNinePandigital n = (and $ map (\x -> x `elem` n) $ ['1'..'9'])
                          && length n == 9

stringOfDigitsOfProducts :: Int -> [Int] -> String
stringOfDigitsOfProducts n xs = concat $ map show [n * x | x <- xs]

main = print $ maximum nums
  where
    numsl = filter isOneToNinePandigital $ [ stringOfDigitsOfProducts x [1..n]
                                           | x <- [1..100000]
                                           , n <- [2..9]]
    nums = map read $ numsl :: [Int]

