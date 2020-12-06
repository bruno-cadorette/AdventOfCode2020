module Day6 where
import Data.List
import Data.List.Split
parseFile = splitOn [""] . lines
day6p1 = sum . fmap (length . nub . concat) . parseFile
day6p2 = sum . fmap (length . foldr1 intersect) . parseFile
