module Main where
import Data.List.Unique
import Data.List.Split
import qualified Data.IntMap as Map
import Debug.Trace (trace)

type FishMap = Map.IntMap Int

-- NOTE: Originally I tried a brute-force solution. This worked for 80 iterations
--       but timed out at around 160. After some thinking I realized I could just
--       put the fish counts into a map and update things that way. Old code is
--       gone now, but I *did* take that approach initially.

updateFishMap :: FishMap -> FishMap
updateFishMap fish =
    fish'
    where
        zeroCt = fish Map.! 0
        fish0 = foldl (\cur index -> Map.insert index (cur Map.! (index + 1)) cur) fish [0..7]
        fish1 = Map.adjust (+ zeroCt) 6 fish0
        fish2 = Map.insert 8 zeroCt fish1
        fish' = fish2

updateFishMapN :: Int -> FishMap -> FishMap
updateFishMapN n map0 =
    last $ take (n + 1) (iterate updateFishMap map0)

initFishMap :: [Int] -> FishMap
initFishMap fish =
    Map.union (Map.fromList $ count fish) (Map.fromList $ zip [0..8] (repeat 0))

readInt :: String -> Int
readInt = read

solveWithIterations :: Int -> [Int] -> Int
solveWithIterations n fish =
    sum $ Map.elems $ updateFishMapN n $ initFishMap fish

main :: IO ()
main = do
    contents <- getContents 
    let fish0 = map readInt $ filter (/="") $ splitOn "," contents
    print $ solveWithIterations 80 fish0
    print $ solveWithIterations 256 fish0

