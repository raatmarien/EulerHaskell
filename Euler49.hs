import Data.List

arePermutations :: Int -> Int -> Int -> Bool
arePermutations n1 n2 n3 = (length ((show n1) \\ (show n2))) == 0
                           && (length ((show n2) \\ (show n3))) == 0

isPrime :: Int -> Bool
isPrime n = not $ or $ [ n `mod` x == 0
                       | x <- [2..(round (sqrt (fromIntegral n)))]]

main = print $ head $ [ let n1 = start
                            n2 = start + add
                            n3 = start + 2 * add
                        in show n1 ++ show n2 ++ show n3
                        | start <- [1000..]
                        , add   <- [1..((10000 - start) `div` 2)]
                        , arePermutations start (start + add) (start + 2 * add)
                        , isPrime start
                        , isPrime (start + add)
                        , isPrime (start + 2 * add)
                        , start /= 1487]
