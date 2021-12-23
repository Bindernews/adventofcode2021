module Main where

import System.Environment
import Control.Monad
import Data.List.Split
import Data.List (transpose)

translateCommand :: String -> [Int]
translateCommand line =
    case word of
        "up" -> [0, -num]
        "down" -> [0, num]
        "forward" -> [num, 0]
    where
        [word,numStr] = splitOn " " line
        num = read numStr :: Int

part1 commands = 
    sum movesX * sum movesY
    where
        [movesX, movesY] = transpose $ map translateCommand commands

foldDepth :: Int -> (Int, Int) -> Int
foldDepth y (move, aim) = y + (move * aim)

part2 commands =
    finalX * finalY
    where
        [dMove, dAim] = transpose $ map translateCommand commands
        aims = scanl (+) 0 dAim
        finalX = sum dMove
        finalY = foldl foldDepth 0 $ zip dMove aims

main :: IO ()
main = do
    contents <- getContents 
    print $ part1 $ lines contents
    print $ part2 $ lines contents

