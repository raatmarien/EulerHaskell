import qualified Data.Set as S
import Data.List

isOneToNinePandigital :: String -> Bool
isOneToNinePandigital n = (and $ map (\x -> x `elem` n) $ ['1'..'9'])
                          && length n == 9

main = print $ S.foldl (+) 0 $ S.fromList $ [a * b
                                            | a <- [1..10000]
                                            , b <- [1..10000]
                                            , isOneToNinePandigital $
                                              concat [(show a), (show b)
                                                     , (show (a*b :: Int))]
                                            ]
