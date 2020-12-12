module Day10 where
import Debug.Trace
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Control.Monad.State


parseFile = fmap read . lines

day10p1 file = (joltDiff 1 + 1) * (joltDiff 3 + 1)
  where 
    xs = fmap (\[a,b] -> (a, b - a)) $ divvy 2 1 $ originalData
    joltDiff n = length $ filter (\(_, b) -> b == n) xs
    originalData = sort $ parseFile file

day10p2 file = evalState answer mempty
  where
    answer = fmap succ $ solve 0 $ sort $ parseFile file
    
type Cache a = State (Map.Map Int Int) a

combine :: [Maybe Int] -> Int
combine xs =  
  let xs' = catMaybes xs 
  in max (length xs' - 1) 0 + (sum xs')

cacheComputation :: (Int -> Cache Int) -> Int -> Cache Int
cacheComputation f key = do
  cachedValue <- gets (Map.lookup key)
  case cachedValue of
    Just x -> return x
    Nothing -> do
      newVal <- f key
      modify (Map.insert key newVal)
      return newVal
    
numberOfPath :: Int -> Int -> [Int] -> Cache (Maybe Int)
numberOfPath a b xs = 
  if b - a <= 3 then do
    fmap Just $ cacheComputation (\b -> solve b xs) b
  else
    return Nothing

solve :: Int -> [Int] -> Cache Int
solve a (b:c:d:xs) = fmap combine $ sequence [numberOfPath a b (c:d:xs), numberOfPath a c (d:xs), numberOfPath a d xs]
solve a (b:c:xs) = fmap combine $ sequence [numberOfPath a b (c:xs), numberOfPath a c xs]
solve _ _ = return 0
