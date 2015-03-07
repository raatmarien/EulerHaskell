import Data.List

wordValue :: String -> Int
wordValue xs = sum $ map charValue xs
  where
    charValue c = head $ [ fst n
                         | n <- (zip [1..26] ['A'..'Z']) 
                         , (snd n) == c]

triangularNumbers :: [Int]
triangularNumbers = map (\n -> (n * (n + 1)) `div` 2) [1..]

isTriangular :: Int -> Bool
isTriangular n = isTriangular' n 0
  where
    isTriangular' n c
      | t == n    = True
      | t > n     = False
      | otherwise = isTriangular' n $ c + 1
      where
        t = triangularNumbers !! c

main = do
  words <- fmap read $ readFile "Euler42_words.txt" :: IO [String]
  print $ length $ filter isTriangular $ map wordValue words
  return ()
