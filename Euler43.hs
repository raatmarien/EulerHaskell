import Data.List

zeroToNinePandigitals :: [String]
zeroToNinePandigitals = filter (\xs -> head xs /= '0') $ permutations ['0'..'9']

primes :: [Int]
primes = [2,3,5,7,11,13,17]

hasDivProperties :: String -> Bool
hasDivProperties xs = hasDivProperties' 1 10 xs 
  where
    hasDivProperties' c m s
      | c > (m - 3)                      = True
      | x `mod` (primes !! (c - 1)) == 0 = hasDivProperties' (c+1) m s
      | otherwise                        = False
      where
        x = read $ map (s !!) [c..(c+2)] :: Int

main = print $ sum $ map read $ filter hasDivProperties zeroToNinePandigitals
