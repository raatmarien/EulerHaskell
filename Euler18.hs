import Data.List
import qualified Data.Map as M

piramid :: [Int]
piramid = map read $ words $ "75 95 64 17 47 82 18 35 87 10 20 04 82 47 65 19 01 23 75 03 34 88 02 77 73 07 63 67 99 65 04 28 06 16 70 92 41 41 26 56 83 40 80 70 33 41 48 72 33 47 32 37 16 94 29 53 71 44 65 25 43 91 52 97 51 14 70 11 33 28 77 73 17 78 39 68 17 57 91 71 52 38 17 14 91 43 58 50 27 29 48 63 66 04 68 89 53 67 30 73 16 69 87 40 31 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

places = [ (x, y) | y <- [0..14], x <- [-14..14], (abs x) <= y, (x `mod` 2) == (y `mod` 2)]

piramidMap :: M.Map (Int, Int) Int
piramidMap = M.fromList $ zip places piramid

addPrevCost :: M.Map (Int, Int) Int -> (Int, Int) -> Int -> Int
addPrevCost map (x, y) value = (max l r) + value
  where
    l = if M.member (x-1,y-1) map then map M.! (x-1, y-1) else 0
    r = if M.member (x+1,y-1) map then map M.! (x+1, y-1) else 0

dive :: Int -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
dive h currentMap = M.union newMap currentMap
  where
    adjustAll map x
      | (abs x) > h = map
      | otherwise   = adjustAll map' (x+2)
      where
        map' = M.adjustWithKey (addPrevCost map) (x, h) map
    newMap = adjustAll currentMap $ (-h)

diving :: Int -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
diving 15 currentMap = currentMap
diving h  currentMap = diving (h+1) $ dive h currentMap

-- main = print $ M.foldl (\max value -> if value > max
--                                       then value
--                                       else max) 0 finMap
--   where
--     finMap = diving 1 piramidMap
