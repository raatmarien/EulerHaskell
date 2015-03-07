import Data.List

isPrime :: Int -> Bool
isPrime n = not $ or $ [ n `mod` x == 0
                       | x <- [2..(round (sqrt (fromIntegral n)))]]

circleMutations :: [a] -> [[a]]
circleMutations xs = circleMutations' xs (length xs)
  where
    circleMutations' _ 0 = []
    circleMutations' xs' l = let circled = (last xs'):(init xs')
                             in circled:(circleMutations' circled (l - 1))

isCircularPrime :: Int -> Bool
isCircularPrime n = and $ map isPrime circledMutations
  where
    circledMutations = map read $ circleMutations $ show n :: [Int]

main = print $ length $ filter isCircularPrime [2..999999]
