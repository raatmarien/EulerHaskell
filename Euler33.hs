factorial :: Int -> Int
factorial n = product [1..n]

isSumOfFactorials :: Int -> Bool
isSumOfFactorials n = n == (sum $ map factorial $ (map read $ map (\x -> [x]) $ show n)) 

main = print $ sum $ filter isSumOfFactorials [3..1000000]
