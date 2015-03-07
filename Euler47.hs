import Data.List

primeNums :: [Int]
primeNums = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

primeFactors :: Int -> [Int]
primeFactors n = p:(if r == 1 then [] else primeFactors r)
  where
    (p, r) = head [ (p, n `div` p)
                  | p <- primeNums
                  , n `mod` p == 0]

-- primeFactorsGrouped :: [Int]
primeFactorsGrouped n = map mult $ group $ primeFactors n
  where
    mult [x]    = x
    mult (x:xs) = x * (mult xs)

prevAreDif :: [Int] -> Bool
prevAreDif xs = (length o1) == 4 && (length o2) == 4
                && (length o3) == 4
                && (length (xs `intersect` o1)) == 0
                && (length (o1 `intersect` o2)) == 0
                && (length (o2 `intersect` o3)) == 0
  where
    n = product xs
    o1 = primeFactorsGrouped (n-1)
    o2 = primeFactorsGrouped (n-2)
    o3 = primeFactorsGrouped (n-3)

main = print $ head $ filter prevAreDif
       $ filter (\n -> (length n) == 4)
       $ map primeFactorsGrouped [2..]
