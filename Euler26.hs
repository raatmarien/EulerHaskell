-- I know, I'm not proud of this
import Data.List.Split

repitionLength :: String -> Int
repitionLength str = rLength (reverse str) 1
  where
    rLength rstr l
      | l >= ((length rstr) `div` 2) = 0
      | otherwise = if last == sLast
                    then l
                    else rLength rstr (l + 1)
        where
          bitsLListM str splitLength current
            | current < splitLength = (splitEvery l str)
                                      : (bitsLListM str' splitLength (current + 1))
            | otherwise             = [] 
            where
              (_: str') = str
          bitsL = splitEvery l rstr
          last = head bitsL
          sLast = bitsL !! 1

-- longestRepeatingFraction :: Double -> Double -> Int -> Double -> Double
longestRepeatingFraction n max best bestN
  | n == max  = (bestN, best)
  | otherwise = if r > best
                then longestRepeatingFraction (n + 1) max r n
                else longestRepeatingFraction (n + 1) max best bestN
  where
    repitionLen n' m max bestL
      | m == max = bestL
      | otherwise = if l > bestL
                    then repitionLen n' (m+1) max l
                    else repitionLen n' (m+1) max bestL
      where
        frac = (10 ^ m) `div` n' :: Integer
        -- [_, decs] = splitOn "." $ show frac
        decs = show frac
        l = repitionLength decs
    n' = round n :: Integer
    r = repitionLen n' 0 5000 0

main = print $ longestRepeatingFraction 2 1000 0 0
