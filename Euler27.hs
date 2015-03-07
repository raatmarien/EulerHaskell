import Data.Numbers.Primes

consecutivePrime :: (Int -> Int) -> Int
consecutivePrime f = consecutive (f) (0)
  where
    consecutive (f') (n) = if isPrime $ (f' n) then consecutive (f') (n + 1) else n - 1

secondPolynomial :: Int -> Int -> (Int -> Int)
secondPolynomial a b = (\n -> (n ^ 2) + a * n + b)

mostPrimes :: (Int, (Int, Int)) -> (Int, Int) -> (Int, (Int, Int))
mostPrimes (bestPrimes, (bestA, bestB)) (a, b) = if primes > bestPrimes
                                                 then (primes, (a, b))
                                                 else (bestPrimes, (bestA, bestB))
  where
    f = secondPolynomial a b
    primes = consecutivePrime f

main = print $ product 
  where
    (bestscore, (a, b)) = foldl mostPrimes (0, (-1, -1)) $ [ (a, b)
                                                           | a <- [-999..999]
                                                           , b <- [-999..999]]
    product = a * b
