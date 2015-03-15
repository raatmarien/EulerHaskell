import Data.List

isPrime :: Int -> Bool
isPrime n = not $ or $ [ n `mod` x == 0
                       | x <- [2..(round (sqrt (fromIntegral n)))]]

-- primeSpiralDiagonals :: Float -> Float
primeSpiralDiagonals frac = spiralDiagonals' 0 0 (0,0)
  where
    spiralDiagonals' 0       prevNum (p,t) = spiralDiagonals' 1 3 (1,2)
    spiralDiagonals' current prevNum (p,t)
      | isFin
        && tF < frac = (t `div` 2) + 1 
      | otherwise    = (spiralDiagonals' (current + 1) newNum
                        (p+(if isPrime newNum then 1 else 0), t+1))
      where
        newNum = (prevNum + ((current+4) `div` 4) * 2)
        isFin = t `mod` 4 == 1
        tF = (fromIntegral p) / (fromIntegral t)
                  

main = print $ primeSpiralDiagonals 0.10 
