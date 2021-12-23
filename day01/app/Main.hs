module Main where

import System.Environment
import Control.Monad

getGroups :: [a] -> Int -> [[a]]
getGroups nums groupSize =
    map conv range
    where 
        range = [0..length nums - groupSize]
        conv = \ i -> let (_,xs) = splitAt i nums in take groupSize xs

isIncrease (a:b:xs) = 
    if a < b then 1 else 0
isIncrease [_] = 0
isIncrease [] = 0

part1 nums =
    sum $ map isIncrease $ getGroups nums 2

part2 :: [Integer] -> Integer
part2 nums = 
    sum $ map isIncrease $ getGroups nums' 2
    where
        nums' = map sum $ getGroups nums 3

main :: IO ()
main = do
    content <- getContents
    let nums = map (\x -> read x :: Integer) (lines content)
    print $ part1 nums
    print $ part2 nums
