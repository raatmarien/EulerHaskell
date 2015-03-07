rightTrianglesWithPerimeter :: Int -> Int
rightTrianglesWithPerimeter p = (length allTriangles) `div` 2
  where
    allTriangles = [ (a, b, c) | a <- [1..(p-2)], b <- [1..(p-2)]
                               , let c = sqrt (fromIntegral (a^2 + b^2))
                                     c'= round c
                               , c == (fromIntegral c')
                               , a + b + c' == p]

main = print $ fst $ foldl (\(bestP, bestS) p -> let s = rightTrianglesWithPerimeter p in if s > bestS
                                                                                          then (p, s)
                                                                                          else (bestP, bestS)) (0,0) [3..1000]
