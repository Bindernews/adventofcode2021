module Main where

import System.Environment
import Control.Monad
import Data.List (transpose, foldl')
import Data.Char (digitToInt)
import Debug.Trace (trace)

-- See https://stackoverflow.com/questions/19554984
count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

-- See https://stackoverflow.com/questions/5921573
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- Check if there are more 0s or 1s
count01 :: String -> Char
count01 col =
    result
    where
        count0 = count '0' col
        count1 = count '1' col
        result = if count1 >= count0 then '1' else '0'
        -- result = trace ("0s: " ++ show count0 ++ " 1s: " ++ show count1 ++ " -> " ++ [result']) result'

-- Invert a 0/1 char
invChar :: Char -> Char
invChar c = if c == '0' then '1' else '0'

-- Invert a string of 0/1 characters
bitStrInvert :: String -> String
bitStrInvert = map invChar

part1 :: [String] -> Int
part1 rows =
    gamma * epsilon
    where
        -- Select bit for each column
        colBits = map count01 $ transpose rows
        gamma = toDec colBits
        epsilon = toDec $ bitStrInvert colBits

-- Implement reduction pattern for part 2
reduceNumbers :: Bool -> Int -> [String] -> Int
reduceNumbers countMost index rows =
    if length remainRows == 1 then
        toDec $ head remainRows
    else
        reduceNumbers countMost (index + 1) remainRows
    where
        colPick = count01 $ transpose rows !! index
        colPick' = if countMost then colPick else invChar colPick
        remainRows = filter (\x -> x!!index == colPick') rows
        -- remainRows = trace (show remainRows0) remainRows0

part2 :: [String] -> Int
part2 rows =
    let oxygen = reduceNumbers True 0 rows
        co2    = reduceNumbers False 0 rows
    in  oxygen * co2

main :: IO ()
main = do
    contents <- getContents 
    let rows = lines contents
    print $ part1 rows
    print $ part2 rows
