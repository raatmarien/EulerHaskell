hasSpecialForm :: Int -> Bool
hasSpecialForm n = n > 10 ^ 18
                   && (n `mod` (n `div` 10 ^ 1) == 0)
                   && ((n `div` 10 ^ 2) `mod` (n `div` 10 ^ 3) == 9)
                   && ((n `div` 10 ^ 4) `mod` (n `div` 10 ^ 5) == 8)
                   && ((n `div` 10 ^ 6) `mod` (n `div` 10 ^ 7) == 7)
                   && ((n `div` 10 ^ 8) `mod` (n `div` 10 ^ 9) == 6)
                   && ((n `div` 10 ^ 10) `mod` (n `div` 10 ^ 11) == 5)
                   && ((n `div` 10 ^ 12) `mod` (n `div` 10 ^ 13) == 4)
                   && ((n `div` 10 ^ 14) `mod` (n `div` 10 ^ 15) == 3)
                   && ((n `div` 10 ^ 16) `mod` (n `div` 10 ^ 17) == 2)
                   && n `div` 10 ^ 18 == 1

main = print $ head $ filter (hasSpecialForm . (^2)) [(10 ^ 9)..]
