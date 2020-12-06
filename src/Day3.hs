module Day3 where

import Data.List
import Data.Maybe
import Control.Monad
import Data.Ord
import Data.Bits
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map as Map

slopePositions maxX maxY (slopeX, slopeY) = 
  takeWhile (\(a, b) -> a < maxX) $ iterate (\(a, b) -> ((a + slopeX), (b + slopeY) `mod` maxY)) (0,0)
widthInput = 31
heightInput = 323
removeLineBreak = filter (\x -> x /= '\n') 
numberOfTrees area slope = length $ filter (== '#') $ fmap (area !) $ slopePositions heightInput widthInput slope

solveDay3 input slopes = product $ fmap (numberOfTrees area) slopes
    where 
        area = listArray ((0,0), (heightInput - 1, widthInput - 1)) $ removeLineBreak input
        
day3p1 input = solveDay3 input [(1, 3)]
        
day3p2 input = solveDay3 input [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]