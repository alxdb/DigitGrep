module Main where

import           AI.NeuralNet
import           Data.Mnist
import           Numeric.LinearAlgebra

main :: IO ()
main = do
    network <- loadNetwork "network"
    testData <- getTestingData
    let results = zipWith (test network) (map snd testData) (map fst testData)
    putStrLn $ "accuracy = " ++ show ((fromIntegral . length . filter (== True) $ results) / (fromIntegral . length $ results) :: Double)

test :: NeuralNet -> Vector R -> Vector R -> Bool
test network expected input = expVal == recVal
  where
    recieved = run network input
    expVal = length . takeWhile (/= 1.0) $ toList expected
    recVal = length . takeWhile (/= (maximum . toList $ recieved)) $ toList recieved
