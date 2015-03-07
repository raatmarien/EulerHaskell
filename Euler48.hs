main = print $ drop ((length sumStr) - 10) sumStr
  where
    sumStr = show $ sum $ map (\n -> n ^ n) [1..1000]
