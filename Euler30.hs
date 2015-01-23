isSumOfDigitsToPower :: Int -> Int -> Bool
isSumOfDigitsToPower p n = n == (sum $ map (^p) digits)
  where
    digits = map read $ map (\n -> [n]) $ show n :: [Int]

main = print $  filter (isSumOfDigitsToPower 5) [2..10000000]
