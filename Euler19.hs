import Data.List

isSunday :: Int -> Bool
isSunday n = n `mod` 7 == 5

leapYears :: [Bool]
leapYears = concat $ replicate 25 [False, False, False, True]

months :: [Int]
months = [31,28,31,30,31,30,31,31,30,31,30,31]

months' :: [Int]
months' = [31,29,31,30,31,30,31,31,30,31,30,31]

allMonths :: [Int]
allMonths = concat [ if leap then months' else months | leap <- leapYears]

timesSundayFold :: (Int, Int) ->  Int -> (Int, Int)
timesSundayFold (times, days) edays = ((if (isSunday days) then times+1 else times), days + edays)

main = print $ fst $ foldl timesSundayFold (0,1) allMonths
