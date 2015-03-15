factorial :: Integral a => a -> a
factorial n = product [1..n]

nCr :: Integral a => a -> a -> a
nCr n r = (factorial n) `div` ((factorial r) * (factorial (n - r)))

main = print $ length [ 0
                      | n <- [1..100]
                      , r <- [2..n-1]
                      , (nCr n r) > 10 ^ 6]
