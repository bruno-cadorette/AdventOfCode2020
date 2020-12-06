module Day5 where
import Data.Bifunctor
import Data.List
import Data.List.Split

data Search = Range Int Int deriving (Show, Eq)

upperHalf (Range a b) = 
  let a' = 1 + ((a + b) `div` 2)
  in Range a' b
      

lowerHalf (Range a b) = 
  let b' = (a + b) `div` 2
  in Range a b'

findSeatId (Range a b, Range c d)
  | a == b && c == d = a * 8 + c
  | otherwise = error "Bad range"

allSeats = fmap (findSeatId . foldl (flip solve) (Range 0 127, Range 0 7))

day5p1 = maximum . allSeats
day5p2 = filter (\[a,b,c] -> c - b > 1 || b - a > 1) . divvy 3 1 . sort . allSeats

solve :: Char -> (Search, Search) -> (Search, Search)
solve c = 
  case c of
    'F' -> first lowerHalf
    'B' -> first upperHalf
    'L' -> second lowerHalf
    'R' -> second upperHalf