import Data.List

sameDigits :: String -> Int -> Bool
sameDigits xs n = (length xs) == (length xs')
                  && (length $ xs \\ xs') == 0
  where
    xs' = show n

multiplesSameDigits :: Int -> Bool
multiplesSameDigits n = and $ map
                        (\x -> sameDigits xs
                               $ n * x) [2..6]
  where
    xs = show n

main = print $ head
       $ filter multiplesSameDigits [1..]
       
