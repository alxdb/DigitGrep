module Main where

import           AI.NeuralNet
import           Data.Mnist
import           Numeric.LinearAlgebra

seed :: Int
seed = 123

main :: IO ()
main = do
    -- Initialization
    trainData <- getTrainingData
    let layerSizes = [size . fst . head $ trainData, 16, 16, 10]
    let initNetwork = generateNeuralNet seed layerSizes
    -- Training Loop
    let network = train initNetwork 0.1 trainData
    saveNetwork "network" network
