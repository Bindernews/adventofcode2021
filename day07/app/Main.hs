module Main where

import Data.List.Split
import Data.IntMap ((!))
import qualified Data.IntMap as Map
import Debug.Trace (trace)

-- Plan: calculate cost at 25%, 50%, 75% (between min-max)
--  whichever of these 3 is the lowest, take that half of the
--  range and repeat.
--  e.g. if 25% is lowest, repeat with range 0% - 50%,
--       if 50% is lowest, repeat with range 25% - 75%
--  Once we have fewer than 10 items, scan each one to find the lowest

buildFrequencyMap :: [Int] -> Map.IntMap Int
buildFrequencyMap =
    foldl (\map' n -> Map.insertWith (+) n 1 map') Map.empty

-- Simple midpoint function
midpoint :: Int -> Int -> Int
midpoint a b = (a + b) `div` 2

readInt :: String -> Int
readInt = read

type CrabMap = Map.IntMap Int

-- Find the location that takes the lowest amount of fuel
findBestLocation :: (Int -> CrabMap -> Int) -> (Int, Int) -> CrabMap -> Int
findBestLocation fuelFn (low, high) crabs
    -- Test each fuel value for best result
    | high - low <= 8         = 
        let (bestFuel,_) = minimum $ fuelsForPositions [low..high]
        in  bestFuel
    -- General case, find lowest among 25%, 50%, 75% and recurse
    | otherwise               =    
        let pos50 = midpoint low high
            pos25 = midpoint low pos50
            pos75 = midpoint pos50 high
            quarter = (high - low) `div` 4 + 1
            (_,bestPos) = minimum $ fuelsForPositions [pos25, pos50, pos75]
        in  findBestLocation fuelFn (max (bestPos - quarter) low, min (bestPos + quarter) high) crabs
    where
        fuelsForPositions positions = map (\ pos -> (fuelFn pos crabs, pos) ) positions

fuelSimple :: Int -> CrabMap -> Int
fuelSimple loc crabs =
    sum $ map (\ (pos, count) -> count * abs (loc - pos)) $ Map.toAscList crabs

fuelMoreCostly :: Int -> CrabMap -> Int
fuelMoreCostly loc crabs =
    sum $ map fuelFn $ Map.toAscList crabs
    where
        fuelFn (pos, count) = 
            let n = abs (loc - pos)
                total = n * (n + 1) `div` 2
            in  count * total
        
main :: IO ()
main = do
    contents <- getContents 
    let crabs = buildFrequencyMap $ map readInt $ splitOn "," contents
    let lowHigh = (minimum $ Map.keys crabs, maximum $ Map.keys crabs)
    print $ findBestLocation fuelSimple lowHigh crabs
    print $ findBestLocation fuelMoreCostly lowHigh crabs
