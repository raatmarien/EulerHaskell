import Data.List

reverseInt :: Integer -> Integer
reverseInt n = read $ reverse $ show n

isPalindromInt :: Integer -> Bool
isPalindromInt n = (reverseInt n) == n

isLychrel :: Integer -> Bool
isLychrel n = isLychrel' 0 $ n + reverseInt n
  where
    isLychrel' c n'
      | c == 48   = True
      | lychrel   = False
      | otherwise = isLychrel' (c+1) newN
      where
        revN = reverseInt n'
        lychrel = revN == n'
        newN = n' + revN

main = print $ length $ filter isLychrel [1..9999]
