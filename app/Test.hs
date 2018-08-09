module Main where

import           AI.NeuralNet
import           Data.Mnist
import           Numeric.LinearAlgebra
import Control.Monad

main :: IO ()
main = do
    network <- loadNetwork "network"
    testData <- getTestingData
    zipWithM_ test (map snd testData) (map (run network . fst) testData)
    print "done"

test :: Vector R -> Vector R -> IO ()
test expected recieved = do
    print expected
    print recieved
    print $ "expected " ++ show (length . takeWhile (/= 1.0) $ toList expected)
    print $ "recieved " ++ show (length . takeWhile (/= (maximum . toList $ recieved)) $ toList recieved)
