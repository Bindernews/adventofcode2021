module Main where

import Data.List
import Data.Either
import Data.Maybe


syntaxComplete :: String -> String -> Either (Char, String) String
syntaxComplete ln stack =
    if null ln' || isLeft stack'
        then stack'
        else syntaxComplete ln' (fromRight "" stack')
    where
        (nc:ln') = ln
        closeChar = fromMaybe ' ' $ listToMaybe stack
        stack' = case nc of
            '(' -> Right (")" ++ stack)
            '{' -> Right ("}" ++ stack)
            '[' -> Right ("]" ++ stack)
            '<' -> Right (">" ++ stack)
            c | c == closeChar -> Right (tail stack)
            _ -> Left (nc, stack)

scoreFail :: Char -> Int
scoreFail c = case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _ -> 0

scoreAuto :: Int -> String -> Int
scoreAuto score (c:xs) = 
    scoreAuto score' xs
    where
        add = case c of
            ')' -> 1
            ']' -> 2
            '}' -> 3
            '>' -> 4
            _ -> 0
        score' = score * 5 + add
scoreAuto score [] = score

part1 :: [String] -> IO ()
part1 lines' = do
    -- mapM_ print $ lefts . map (`syntaxComplete` "") lines'
    let fails = map fst $ lefts $ map (`syntaxComplete` "") lines'
    print $ sum $ map scoreFail fails

part2 :: [String] -> IO ()
part2 lines' = do
    let completed = rights $ map (`syntaxComplete` "") lines'
    -- mapM_ print completed
    let scores = sort $ map (scoreAuto 0) completed
    print $ scores !! (length scores `div` 2)

main :: IO ()
main = do
    contents <- getContents
    let lines' = lines contents
    part1 lines'
    part2 lines'

