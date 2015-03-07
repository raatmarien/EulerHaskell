import qualified Data.Map as M
import Data.List

basicNumbersMap :: M.Map Int String
basicNumbersMap = M.fromList $ zip [0..19] ["", "one", "two", "three", "four", "five"
                                           , "six", "seven", "eight", "nine", "ten"
                                           , "eleven", "twelve", "thirteen", "fourteen", "fifteen"
                                           , "sixteen", "seventeen", "eighteen", "nineteen"]

tensMap :: M.Map Int String
tensMap = M.union (M.fromList $ zip [2,3,4,5,8] $ ["twenty", "thirty", "forty", "fifty", "eighty"])
          (M.fromList (zip [2..9] $ zipWith (++) [basicNumbersMap M.! n | n <- [2..9]] (repeat "ty")))

toEnglishString :: Int -> String
toEnglishString num = toEnglishString' num False
  where
    toEnglishString' n prev
      | n == 0    = ""
      | n >= 1000 = (basicNumbersMap M.! (n `div` 1000)) ++ " thousand" ++ (toEnglishString' (n `mod` 1000) True)
      | n >= 100  = (if prev then " " else "") ++ (basicNumbersMap M.! (n `div` 100) ++ " hundred" ++ (toEnglishString' (n `mod` 100) True))
      | n >= 20   = (if prev then " and " else "") ++ (tensMap M.! (n `div` 10))
                    ++ if n `mod` 10 /= 0 then "-" ++ (toEnglishString' (n `mod` 10) False) else ""
      | otherwise = (if prev then " and " else "") ++ (basicNumbersMap M.! n)

main = print $ length $ filter (\n -> n /= ' ' && n /= '-') $ concat $ map toEnglishString [1..1000]
