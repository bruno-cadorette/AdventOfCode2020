module Day9 where

import Data.Maybe
import Data.List
import Data.List.Split

parseFile :: String -> [Int]
parseFile = fmap read . lines

extract = mapMaybe uncons . tails

containsSubSum :: Int -> [Int] -> Bool
containsSubSum n =
  any (\(x, xs) -> any (\y -> x + y == n) xs) . extract

consecutiveSum :: Int -> Int -> [Int] -> Maybe [Int]
consecutiveSum goal acc (x:xs)
  | acc + x == goal = Just [x]
  | acc + x < goal = ( x: ) <$> consecutiveSum goal (acc + x) xs
consecutiveSum goal acc _ = Nothing

day9p1 = fromJust . fmap head . find (\(x:xs) -> not $ containsSubSum x xs) . reverse . divvy 26 1 . reverse . parseFile

day9p2 file = fmap (\list -> minimum list + maximum list) $ filter (\list -> length list > 1) $ mapMaybe (consecutiveSum goal 0) $ tails xs
  where
    goal = day9p1 file
    xs = parseFile file