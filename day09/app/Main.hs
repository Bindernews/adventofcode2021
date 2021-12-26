module Main where

import Data.List
import qualified Data.Set as Set
import Debug.Trace (trace)
import Control.Monad (forM)

type Board = [[Int]]

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq, Ord)

toTuple :: Point -> (Int, Int)
toTuple pt = (x pt, y pt)

offset :: (Int, Int) -> Point -> Point
offset (dx, dy) pt = Point { x = x pt + dx, y = y pt + dy }

-- Get the value at the given location in the board, with some default
-- if the location is out of bounds.
boardGet :: Int -> (Int, Int) -> Board -> Int
boardGet default' (x, y) board =
    if x < 0 || y < 0 || y >= length board
        then default'
        else
            let row = board !! y
            in  if x >= length row then default' else row !! x

isInBoard :: Point -> Board -> Bool
isInBoard pt board =
    boardGet 10 (toTuple pt) board < 10

isLowest :: Point -> Board -> Bool
isLowest pt board =
    (mid < x0) && (mid < x1) && (mid < y0) && (mid < y1)
    where
        bget x y = boardGet 10 (x, y) board
        x0 = bget (x pt - 1) (y pt)
        x1 = bget (x pt + 1) (y pt)
        y0 = bget (x pt) (y pt - 1)
        y1 = bget (x pt) (y pt + 1)
        mid = bget (x pt) (y pt)

findLowest :: Board -> [Point]
findLowest board =
    concatMap (\y ->
        concatMap (`testPt` y) [0..(length (board!!y) - 1)]
    ) [0..(length board - 1)]
    where
        testPt x y =
            let pt = Point { x = x, y = y }
            in  [pt | isLowest pt board]

getBasinSize :: Point -> Board -> Int
getBasinSize pt board =
    impl (Set.singleton pt) [pt]
    where
        impl seen0 todo0 =
            let (pt':todo') = todo0
                pN = offset (0,-1) pt'
                pS = offset (0,1)  pt'
                pE = offset (-1,0) pt'
                pW = offset (1,0)  pt'
                -- Conditionally add a point to seen and todo
                addPoint (seen, todo) pt =
                    if not (Set.member pt seen) && boardGet 9 (toTuple pt) board < 9
                        then (Set.insert pt seen, todo ++ [pt])
                        else (seen, todo)
                (seen', todo'') = foldl addPoint (seen0, todo') [pN, pS, pE, pW]
            in  if null todo''
                    then Set.size seen'
                    else impl seen' todo''

parseBoard :: String -> Board
parseBoard contents =
    map (map readInt) $ lines contents
    where
        readInt s = read [s] :: Int

part1 :: Board -> Int
part1 board =
    sum $ map (\pt -> boardGet 10 (toTuple pt) board + 1) $ findLowest board

part2 :: Board -> IO ()
part2 board = do
    let basinSizes = take 3 $ reverse $ sort $ map (`getBasinSize` board) $ findLowest board
    print (head basinSizes * basinSizes!!1 * basinSizes!!2)
    -- let msgs = map (\pt -> show pt ++ " " ++ show (getBasinSize pt board)) $ findLowest board
    -- mapM_ print msgs

main :: IO ()
main = do
    contents <- getContents
    let board = parseBoard contents
    print $ part1 board
    part2 board
