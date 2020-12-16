{-# OPTIONS_GHC -Wall #-}
import qualified Data.Vector as V
import Control.Monad (join)
import Control.Arrow ((***))

type Map = V.Vector (V.Vector Char)

-- FIXME: this is really slow
main :: IO ()
main = do
  rawInput <- fmap lines getContents
  let input = V.fromList (fmap V.fromList rawInput)
  let finalMap = converge (updateMap adjacentOccupied1 4) input
  print . length . V.filter (== '#') . join $ finalMap
  -- part 2
  let finalMap2 = converge (updateMap adjacentOccupied2 5) input
  print . length . V.filter (== '#') . join $ finalMap2
  return ()

updateMap :: (Map -> (Int, Int) -> Int) -> Int -> Map -> Map
updateMap f n m = fmap (calculateOne f n m) <$> V.fromList (fmap V.fromList allIdx)
  where
  height = length m
  width = length (V.head m)
  allIdx = fmap (\a -> fmap (\b -> (b, a)) [0..(width - 1)]) [0..(height - 1)]

calculateOne :: (Map -> (Int, Int) -> Int) -> Int -> Map -> (Int, Int) -> Char
calculateOne f n m (x, y) = case mapLookup m (x, y) of
  '.' -> '.'
  'L' -> if numAdjOcc == 0 then '#' else 'L'
  '#' -> if numAdjOcc >= n then 'L' else '#'
  _ -> error "bad input"
  where
  numAdjOcc = f m (x, y)

adjacentOccupied2 :: Map -> (Int, Int) -> Int
adjacentOccupied2 m (x, y) = rowCount + colCount + diagCount
  where
  isOcc = (\xs -> if V.null xs || V.head xs == 'L' then 0 else 1) . V.filter (/= '.')
  rowCount = uncurry (+) . ((isOcc . V.reverse) *** (isOcc . V.tail)) . V.splitAt x $ m V.! y
  colCount = uncurry (+) . ((isOcc . V.reverse) *** (isOcc . V.tail)) . V.splitAt y $ fmap (V.! x) m
  height = length m
  width = length (V.head m)
  move (h, v) (x', y') = (x' + h, y' + v)
  diag (h, v) = takeWhile (indexValid (width, height)) $ fmap (\n -> applyNTimes n (move (h, v)) (x, y)) [1..]
  diagCount = sum . fmap ((isOcc . V.fromList) . fmap (mapLookup m) . diag) $ [(1, 1), (1, -1), (-1, 1), (-1, -1)]

adjacentOccupied1 :: Map -> (Int, Int) -> Int
adjacentOccupied1 m (x, y) = length . filter (== '#') . fmap (mapLookup m) . filter (indexValid (width, height)) . adjacentCells $ (x, y)
  where
  height = length m
  width = length (V.head m)

indexValid :: (Int, Int) -> (Int, Int) -> Bool
indexValid (width, height) (x, y)
  | x < 0 || y < 0 = False
  | x >= width || y >= height = False
  | otherwise = True

adjacentCells :: (Int, Int) -> [(Int, Int)]
adjacentCells (x, y) = filter (/= (x, y)) . fmap (\(f, g) -> (f x, g y)) $ (,) <$> [pred, id, succ] <*> [pred, id, succ]

mapLookup :: Map -> (Int, Int) -> Char
mapLookup m (x, y) = (m V.! y) V.! x

converge :: (Eq a) => (a -> a) -> a -> a
converge f x
  | f x == x = x
  | otherwise = converge f (f x)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldr (.) id (replicate n f)
