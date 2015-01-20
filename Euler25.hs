n = (10 ^ 999)

firstFibWith1000Dig :: Integer -> Integer -> Integer -> Integer
firstFibWith1000Dig n1 n2 a
  | fibn >= n = a
  | otherwise = firstFibWith1000Dig n2 fibn (a + 1)
  where
    fibn = n1 + n2

main = print $ firstFibWith1000Dig 1 1 3
