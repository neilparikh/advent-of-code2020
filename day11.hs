{-# OPTIONS_GHC -Wall #-}
import qualified Data.Vector as V
import Control.Monad (join)

type Map = V.Vector (V.Vector Char)

main :: IO ()
main = do
  rawInput <- fmap lines getContents
  let input = V.fromList (fmap V.fromList rawInput)
  -- FIXME: this is really slow
  let finalMap = converge updateMap input
  print . length . V.filter (== '#') . join $ finalMap
  return ()

updateMap :: Map -> Map
updateMap m = fmap (calculateOne m) <$> V.fromList (fmap V.fromList allIdx)
  where
  height = length m
  width = length (V.head m)
  allIdx = fmap (\a -> fmap (\b -> (b, a)) [0..(width - 1)]) [0..(height - 1)]

calculateOne :: Map -> (Int, Int) -> Char
calculateOne m (x, y) = case mapLookup m (x, y) of
  '.' -> '.'
  'L' -> if numAdjOcc == 0 then '#' else 'L'
  '#' -> if numAdjOcc >= 4 then 'L' else '#'
  _ -> error "bad input"
  where
  height = length m
  width = length (V.head m)
  numAdjOcc = adjacentOccupied m (width, height) (x, y)

adjacentOccupied :: Map -> (Int, Int) -> (Int, Int) -> Int
adjacentOccupied m (width, height) (x, y) = length . filter (== '#') . fmap (mapLookup m) . filter (indexValid (width, height)) . adjacentCells $ (x, y)

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
