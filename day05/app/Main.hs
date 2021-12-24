module Main where

import Debug.Trace (trace)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

type Board = [[Int]]
type Point = (Int, Int)
type Line = (Point, Point)

-- Stack Overflow kinda
replaceWith :: (a -> a) -> Int -> [a] -> [a]
replaceWith elemFn index xs =
    let (x,y:ys) = splitAt index xs
    in  x ++ elemFn y : ys

boardReplace :: (Int -> Int) -> Int -> Int -> Board -> Board
-- Args y board are implied
boardReplace f x =
    replaceWith (replaceWith f x)


correctRange :: Int -> Int -> [Int]
correctRange from to =
    let (l, h) = (min from to, max from to)
    in  if from > to then reverse [l..h] else [l..h]

lineToPoints :: Bool -> Point -> Point -> [Point]
lineToPoints allowDiag p1 p2 =
    let (x1, y1) = p1
        (x2, y2) = p2
        (xl, xh) = (min x1 x2, max x1 x2)
    in  if xl == xh then
            zip (repeat xl) (correctRange y1 y2)
        else if y1 == y2 then
            zip [xl..xh] (repeat y1)
        else if allowDiag then
            zip (correctRange x1 x2) (correctRange y1 y2)
        else
            []

boardUpdatePoints :: Board -> [Point] -> Board
boardUpdatePoints =
    foldl (\board' (px,py) -> boardReplace (+ 1) px py board')

parseLine :: String -> Line
parseLine s =
    let nums = getAllTextMatches (s =~ "[0-9]+") :: [String]
        readInt = read :: String -> Int
    in  ( (readInt $ nums!!0, readInt $ nums!!1), (readInt $ nums!!2, readInt $ nums!!3) )

initBoard :: Int -> Board
initBoard size =
    replicate size (replicate size 0)

countSafePoints :: Board -> Int
countSafePoints board =
    let foldBoard = (\accum value -> if value > 1 then accum + 1 else accum)
    in  sum $ map (foldl foldBoard 0) board

part1 :: [Line] -> Int
part1 lines' =
    let allPoints = concatMap (uncurry $ lineToPoints False) lines'
        board1 = boardUpdatePoints (initBoard 999) allPoints
    in  countSafePoints board1

part2 :: [Line] -> Int
part2 lines' =
    let allPoints = concatMap (uncurry $ lineToPoints True) lines'
        board1 = boardUpdatePoints (initBoard 999) allPoints
    in  countSafePoints board1

main :: IO ()
main = do
    contents <- getContents 
    let lines' = map parseLine $ lines contents
    print $ part1 lines'
    print $ part2 lines'

