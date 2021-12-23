module Main where

import System.Environment
import Control.Monad
import Data.List (transpose)

main :: IO ()
main = do
    contents <- getContents 
    let cols = transpose $ lines contents
    print $ head cols

