import qualified Data.Set as S

main = print $ S.size $ S.fromList $ [ a ^ b | a <- [2..100], b <- [2..100]]
