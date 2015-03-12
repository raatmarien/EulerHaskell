import Data.Char
import Data.List

toBits :: Int -> [Int]
toBits n = toBits' 0 7 n
  where
    toBits' c m n'
      | c > m     = []
      | otherwise = (n' `mod` 2)
                    :(toBits' (c+1) m
                      (n' `div` 2))

fromBits :: [Int] -> Int
fromBits xs = foldr (\n x -> 2 * x + n)
              0 xs

xor :: Int -> Int -> Int
xor 0 0 = 0
xor 0 1 = 1
xor 1 0 = 1
xor 1 1 = 0

xorByte :: [Int] -> [Int] -> [Int]
xorByte xs1 xs2 = zipWith xor xs1 xs2

decode :: [Char] -> [Int] -> String
decode pass file = decString
  where
    intPass = map ord pass
    bitPass = concat $ repeat $ map toBits intPass
    bitFile = map toBits file
    decBitFile = zipWith xorByte bitPass bitFile
    decFile = map fromBits decBitFile
    decString = map chr decFile

spacesBy :: String -> Int -> Int
spacesBy xs jump = spacesBy' 0 0
  where
    spacesBy' c p
      | c >= length xs = p
      | otherwise      = spacesBy' (c+jump)
                         newP
      where
        thisChar = xs !! c
        newP = if thisChar == ' '
               then p + 1
               else p

main = do
  encryptedText <- fmap read $ readFile "Euler59_cipher.txt" :: IO [Int]
  let listOfSpacesOne = map (\c -> let pass = c:"aa"
                                   in spacesBy (drop 0
                                                (decode pass
                                                 encryptedText)) 3) ['a'..'z']
      maxSpacesOne = maximum listOfSpacesOne
      charOne = chr ((ord 'a') + (length $ takeWhile (/= maxSpacesOne)
                                  listOfSpacesOne)) 
      listOfSpacesTwo = map (\c -> let pass = 'a':c:"a"
                                   in spacesBy (drop 1
                                                (decode pass
                                                 encryptedText)) 3) ['a'..'z']
      maxSpacesTwo = maximum listOfSpacesTwo
      charTwo = chr ((ord 'a') + (length $ takeWhile (/= maxSpacesTwo)
                                  listOfSpacesTwo))
      listOfSpacesThree = map (\c -> let pass = 'a':'a':[c]
                                     in spacesBy (drop 2
                                                  (decode pass
                                                   encryptedText)) 3) ['a'..'z']
      maxSpacesThree = maximum listOfSpacesThree
      charThree = chr ((ord 'a') + (length $ takeWhile (/= maxSpacesThree)
                                    listOfSpacesThree)) 
      password = charOne:charTwo:charThree:[]
      decodedText = decode password encryptedText
      sumAsciiValue = sum $ map ord decodedText
  print sumAsciiValue
