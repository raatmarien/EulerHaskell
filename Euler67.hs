import Data.List
import qualified Data.Map as M

piramid :: String -> [Int]
piramid triangle = map read $ words $ concat $ lines triangle

places = [ (x, y) | y <- [0..99], x <- [-99..99], (abs x) <= y, (x `mod` 2) == (y `mod` 2)]

piramidMap :: String -> M.Map (Int, Int) Int
piramidMap triangle = M.fromList $ zip places $ piramid triangle

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
diving 100 currentMap = currentMap
diving h   currentMap = diving (h+1) $ dive h currentMap

main = do
  triangle <- readFile "Euler67_triangle.txt"
  let finMap = diving 1 $ piramidMap triangle
  print $ M.foldl (\max value -> if value > max
                                      then value
                                      else max) 0 finMap
