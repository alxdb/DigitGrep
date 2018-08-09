module Main where

import           AI.NeuralNet
import           Data.Mnist
import           Numeric.LinearAlgebra

seed :: Int
seed = 123

eta :: R
eta = 0.1

hiddenLayers :: [Int]
hiddenLayers = [16, 16]

epochs :: Int
epochs = 4

main :: IO ()
main = do
    -- Initialization
    trainData <- getTrainingData
    let layerSizes = [size . fst . head $ trainData] ++ hiddenLayers ++ [size . snd . head $ trainData]
    let initNetwork = generateNeuralNet seed layerSizes
    -- Training Loop
    let network = head . take epochs $ iterate' (train eta trainData) initNetwork
    saveNetwork "network" network

-- Util

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x `seq` (x : iterate' f (f x))
