import qualified Data.Map as M
import Data.List
import Data.Ord

newtype PlaceInt = PlaceInt { getPlaceInt :: ((Int, Int), Int)}
instance PlaceInt where
 pInt1 `compare` pInt2 = if y == y2
                         then x `compare` x2
                         else y `compare` y2
   where
     ((x, y), _) = getPlaceInt pInt1
     ((x2, y2), _) = getPlaceInt pInt2

turnRight :: (Int, Int) -> (Int, Int)
turnRight (dx, dy) = (-dy, dx)

addNumTuple :: Num a => (a, a) -> (a, a) -> (a, a)
addNumTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

generateSpiral :: Int -> (Int, Int) -> (Int, Int) -> Int
                  -> Int -> M.Map (Int, Int) Int
generateSpiral n prevpos direction times left
  | n > (5 ^ 2) = M.singleton pos n
  | left == 0      = M.insert pos n $ generateSpiral (n+1) pos' direction'
                     times' left'
  | otherwise      = M.insert pos n $ generateSpiral (n+1) pos direction times
                     (left - 1)
  where
    direction' = turnRight direction
    pos = addNumTuple prevpos direction
    pos' = addNumTuple prevpos direction'
    times' = times + 1
    left' = ceiling ((fromIntegral times) / 2) :: Int

m = generateSpiral 1 (1, 2) (1, 0) 1 1

main = print $ sort $ M.assocs m
