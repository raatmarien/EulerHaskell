import Data.List

pentagonalNums :: [Int]
pentagonalNums = map (\n -> (n * (3 * n - 1)) `div` 2) [1..]

isNatural :: RealFrac a => a -> Bool
isNatural f = fromInteger (round f) == f

isPentagonal :: Int -> Bool
isPentagonal n = isNatural ((sqrt (24 * fromIntegral n + 1) + 1) / 6)
-- isPentagonal n = isPentagonal' 0 n
--   where
--     isPentagonal' c n
--       | p > n     = False
--       | p == n    = True
--       | otherwise = isPentagonal' (c+1) n
--       where
--         p = pentagonalNums !! c

main = print $ head $ [(abs (p2 - p1), p2, p1)
                      | p1 <- map (pentagonalNums !!) [1..]
                      , p2 <- takeWhile (<p1) pentagonalNums
                      , p1 /= p2
                      , isPentagonal (abs(p2 - p1))
                      , isPentagonal (p2 + p1)]
