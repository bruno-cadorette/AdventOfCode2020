{-# LANGUAGE TupleSections #-}
module Day11 where

import Debug.Trace
import Data.Array
import Data.Bifunctor
import Data.List
import Control.Monad
import Data.Ord
import Data.List.Split
import Data.Maybe

parseSeat '.' = Floor
parseSeat 'L' = EmptySeat
parseSeat '#' = OccupiedSeat

parseLine n = zipWith (\i s -> ((n,i), parseSeat s)) [0..]
parseMap = concat . zipWith parseLine [0..] . lines 

createMap :: [((Int,Int), Seat)] -> Array (Int, Int) Seat
createMap xs = array ((0,0), b) xs
  where
    b = fst $ maximumBy (comparing fst) xs

safeGet :: Ix i => i -> Array i a -> Maybe a
safeGet i arr = 
  if inRange (bounds arr) i then
    Just $ arr ! i
  else
    Nothing

farthest :: (a -> Maybe a) -> a -> a
farthest f = go where
  go a = maybe a go (f a)


data Seat = Floor | EmptySeat | OccupiedSeat deriving (Show, Eq)


adjacentPositions pos = tail $ (\f g -> bimap f g pos) <$> funcs <*> funcs
  where funcs = [id, pred, succ]

countOccupedAdjacents map pos = length $ filter (== (Just OccupiedSeat)) $ fmap (\p -> safeGet p map) $ adjacentPositions pos

seatChange EmptySeat 0 = Just OccupiedSeat
seatChange OccupiedSeat n
  | n >= 4 = Just EmptySeat
seatChange x _ = Nothing

--nextState :: Array (Int,Int) Seat -> Array (Int, Int) Seat
nextState m =    
  if null toModify then 
    Nothing
  else
    Just $ m // toModify 
  where
    toModify = mapMaybe (\(pos, x) -> (pos,) <$> seatChange x (countOccupedAdjacents m pos)) $ assocs m
  

countOccupiedTotal = foldr (\x n -> if x == OccupiedSeat then n + 1 else n) 0 

day11p1 = countOccupiedTotal . farthest nextState . createMap . parseMap


adjacentPositions2 :: [(Int -> Int, Int -> Int)]
adjacentPositions2 = tail $ (,) <$> funcs <*> funcs
  where funcs = [id, pred, succ]

findFirstOccupied :: Array (Int, Int) Seat -> (Int, Int) -> (Int -> Int, Int -> Int) -> Maybe Seat
findFirstOccupied map (x, y) = 
  head . 
  dropWhile (== Just Floor) . 
  fmap (\pos -> safeGet pos map) .
  drop 1 .  
  scanl (\(x,y) (f,g)  -> (f x, g y)) (x,y) .
  repeat

countOccupiedVision map pos = length $ filter (== (Just OccupiedSeat)) $ fmap (findFirstOccupied map pos) adjacentPositions2
    

nextStateP2 m = 
    if null toModify then 
      Nothing
    else
      Just $ m // toModify 
  where
    toModify = mapMaybe (\(pos, x) -> (pos,) <$> seatChangeP2 x (countOccupiedVision m pos)) $ assocs m

seatChangeP2 EmptySeat 0 = Just OccupiedSeat
seatChangeP2 OccupiedSeat n
  | n >= 5 = Just EmptySeat
seatChangeP2 x _ = Nothing

day11p2 = countOccupiedTotal . farthest nextStateP2 . createMap . parseMap


showMap arr = unlines $ fmap (fmap (showSeat. snd)) $ groupBy (\a b -> fst (fst a) == fst (fst b)) $ assocs arr 
  where
    showSeat EmptySeat = 'L'
    showSeat Floor = '.'
    showSeat OccupiedSeat = '#'