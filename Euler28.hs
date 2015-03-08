spiralDiagonals :: Int -> Int -> Int -> Int
spiralDiagonals 0       prevNum max = 4+(spiralDiagonals 1 3 max) 
spiralDiagonals current prevNum max
  | newNum > max = 0
  | otherwise    = newNum+(spiralDiagonals (current + 1) newNum
                                                           max)
  where
    newNum = (prevNum + ((current+4) `div` 4) * 2)

main = print $ spiralDiagonals 0 0 (1001 * 1001)
