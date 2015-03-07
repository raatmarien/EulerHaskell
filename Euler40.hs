import Data.List
import Data.Char

fractionDigitList :: [Char]
fractionDigitList = concat $ map show $ [1..]

main = print $ foldl (*) 1 $ map (digitToInt . (fractionDigitList !!)) $ [0,9,99,999,9999,99999,999999]
