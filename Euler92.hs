import Data.List
import Data.Char

reverseSquareDigitChain :: Int -> [Int]
reverseSquareDigitChain n = squareDigitChain' n []
  where
    squareDigitChain' n' soFar
      | n' `elem` soFar = soFar
      | otherwise      = squareDigitChain'
                         newNum (n':soFar)
      where
        digits = map digitToInt $ show n'
        newNum = sum $ map (\x -> x * x) digits

main = print $ length $ filter (\xs -> 89 `elem` xs)
       $ map reverseSquareDigitChain [1..10000000] 
