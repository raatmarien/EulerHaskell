isNatural :: RealFrac a => a -> Bool
isNatural f = fromInteger (round f) == f

isPentagonal :: Int -> Bool
isPentagonal n = isNatural ((sqrt (24 * fromIntegral n + 1) + 1) / 6)

hexagonalNums :: [Int]
hexagonalNums = map (\n -> n * (2 * n - 1)) [144..]

main :: IO ()
main = print $ head $ [x | x <- hexagonalNums
                         , isPentagonal x]
