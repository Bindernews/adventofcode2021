module Main where

import System.Environment
import Control.Monad
import Data.List.Split
import Data.List
import Debug.Trace (trace)

type Board = [[Int]]
type BoardState = [[Bool]]

-- Stolen from SO
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- Wrote it myself!
mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f inp = map (\i -> f i (inp!!i)) [0..length inp-1]

-- SO again, kinda
replaceWith :: (a -> a) -> Int -> [a] -> [a]
replaceWith elemFn index xs =
    let (x,y:ys) = splitAt index xs
    in  x ++ elemFn y : ys

-- Remove the element at the given index from the list
deleteAt :: Int -> [a] -> [a]
-- deleteAt 0 xs =
--     tail xs
deleteAt index xs =
    let (y,_:ys) = splitAt index xs
    in  y ++ ys

parseRow :: String -> [Int]
parseRow row =
    map (read :: String -> Int) $ filter (/="") (splitOn " " row)

parseBoard :: Int -> [String] -> [[Int]]
parseBoard start xs =
    map parseRow $ slice start (start + 5) xs

boardFind :: Board -> Int -> Maybe (Int, Int)
boardFind board pick =
    let rows = map (elemIndex pick) board
        rowIndex = findIndex (/=Nothing) rows
    in  rowIndex >>= (\y -> rows!!y >>= (\x -> Just (x, y)))

updateBoardState :: BoardState -> Board -> Int -> BoardState
updateBoardState state board pick =
    case boardFind board pick of
    Just (px,py) -> replaceWith (replaceWith (const True) px) py state
    _  -> state

isWinningBoard :: BoardState -> Bool 
isWinningBoard state =
    let rows = map (all (==True)) state
        cols = map (all (==True)) $ transpose state
        -- TODO add diagonals
    in or $ rows ++ cols

scoreBoard :: BoardState -> Board -> Int -> Int
scoreBoard state board pick =
    let rows = map (uncurry zip) $ zip state board
        foldOp = (\ total (used, num) -> total + if used then 0 else num )
        rowSums = map (foldl foldOp 0) $ transpose rows
    in  sum rowSums * pick

findWinningBoard :: [BoardState] -> [Board] -> [Int] -> Int
findWinningBoard state boards picks =
    let (pick:picks') = picks
        state' = mapWithIndex (\x board -> updateBoardState (state!!x) board pick) boards
        winner = elemIndex True $ map isWinningBoard state'
    in  case winner of
        Just index -> scoreBoard (state'!!index) (boards!!index) pick
        _ -> findWinningBoard state' boards picks'


findLastWinningBoard :: [BoardState] -> [Board] -> [Int] -> Int
findLastWinningBoard state boards picks =
    let (pick:picks') = picks
        -- Produce next board states
        state' = mapWithIndex (\x board -> updateBoardState (state!!x) board pick) boards
        -- Remove winning boards
        (state'', boards') = unzip $ filter (\ (s,_) -> not $ isWinningBoard s) (zip state' boards)
    in  if null boards' then
            -- Score the last remaining board
            scoreBoard (head state') (head boards) pick
        else
            findLastWinningBoard state'' boards' picks'

main :: IO ()
main = do
    content <- getContents
    let linesAr = lines content
    let picks = map (read :: String -> Int) (splitOn "," $ head linesAr)
    let boardLines = groupBy (\ _ b -> b /= "") (drop 1 linesAr) :: [[String]]
    let boards = map (parseBoard 1) boardLines
    let states0 = replicate (length boards) $ replicate 5 $ replicate 5 False
    print $ findWinningBoard states0 boards picks
    print $ findLastWinningBoard states0 boards picks

