module Main where

import           Control.Monad (replicateM)
import           Data.IDX
import           Network
import           System.Random

seed :: Int
seed = 314

main :: IO ()
main = do
    setStdGen (mkStdGen seed)
    let layerSizes = [784, 16, 16, 10]
    network <- initialiseNetwork layerSizes :: IO (NetworkData Double)
    input   <- replicateM (head layerSizes) (randomIO :: IO Double)
    let output = runNetwork input network
    print output
    print $ cost output 2
    print $ cost [ if x == 2 then 1.0 else 0.0 | x <- [0 .. (head layerSizes)] ] 2
