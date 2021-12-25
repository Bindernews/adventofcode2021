module Main where

import Data.Sort
import Data.List (elemIndex)
import Data.List.Split
import qualified Data.Set as Set
import Data.Set ((\\))
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)
import Debug.Trace (trace)

{- PLAN:
Segment Counts:
0:6 1:2  2:5  3:5  4:4  5:5  6:6  7:3  8:7  9:6

Segment Map:
 aa
b  c
b  c
 dd
e  f
e  f
 gg

Steps:
1 <- #2
4 <- $4
7 <- #3
8 <- #7
9 <- #6 which is superset of 4
0 <- #6 which is superset of 7 && not 9
6 <- other #6
5 <- #5 which is strict subset of 6
A <- 7 - 1
C <- 8 - 6
D <- 8 - 0
E <- 6 - 5
F <- 1 - C
B <- 4 - 1 - D
G <- 0 - 7 - BE
-}


head' msg xs = head (trace msg xs)

unscrambleSegments :: [String] -> Map String Int
unscrambleSegments notes =
    Map.fromList $ map (\(s,n) -> (Set.toAscList s, n)) numPairs
    where
        notes' = map Set.fromList notes
        withLength n = filter (\s -> Set.size s == n) notes'
        subsetOf = Set.isSubsetOf
        get0 = Set.elemAt 0
        n1 = head $ withLength 2
        n4 = head $ withLength 4
        n7 = head $ withLength 3
        n8 = head $ withLength 7
        len5 = withLength 5
        len6 = withLength 6
        n9 = head $ filter (\s -> subsetOf n4 s) len6
        n0 = head $ filter (\s -> subsetOf n7 s && s /= n9) len6
        n6 = head $ filter (\s -> (s /= n0) && (s /= n9)) len6
        n5 = head $ filter (\s -> subsetOf s n6) len5
        len5' = filter (/= n5) len5
        cC = get0 $ n8 \\ n6
        cF = get0 $ Set.delete cC n1
        n3 = head $ filter (Set.member cF) len5'
        n2 = head $ filter (/= n3) len5'
        numPairs = [(n0, 0), (n1, 1), (n2, 2), (n3, 3), (n4, 4),
            (n5, 5), (n6, 6), (n7, 7), (n8, 8), (n9, 9)]
        
type LinePair = ([String], [String])

linePairToNums :: LinePair -> [Int]
linePairToNums (examples, digits) =
    map (\s -> reverseMap ! (sort s)) digits
    where
        reverseMap = unscrambleSegments examples

parseLine :: String -> LinePair
parseLine line =
    (examples, digits)
    where
        parts = splitOn " " line
        sepIndex = case elemIndex "|" parts of
            Just x -> x
            Nothing -> error ("Invalid input: " ++ line)
        examples = take sepIndex parts
        digits = drop (sepIndex + 1) parts

part1 :: [LinePair] -> Int
part1 lines =
    length $ filter (\a -> a == 1 || a == 4 || a == 7 || a == 8) digits
    where
        digits = concatMap linePairToNums lines

part2 :: [LinePair] -> Int
part2 lines =
    sum outputValues
    where
        arrayToInt ar = (head ar * 1000) + (ar!!1 * 100) + (ar!!2 * 10) + (ar!!3)
        outputValues = map (arrayToInt . linePairToNums) lines

main :: IO ()
main = do
    contents <- getContents 
    let eachLine = map parseLine $ lines contents
    print $ part1 eachLine
    print $ part2 eachLine

