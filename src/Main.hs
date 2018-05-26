module Main where

import           Data.IDX
import           Network

main :: IO ()
main = do
    let output = propagate [[1,4,3],[1,2,3],[1,2,3]] [1,2,3] [0.1, 0.1, 0.1]
    print output
