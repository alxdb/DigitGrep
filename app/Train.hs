module Main where

import           AI.NeuralNet
import           Control.DeepSeq
import           Data.Mnist
import           Numeric.LinearAlgebra

seed :: Int
seed = 123

eta :: R
eta = 0.3

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
    putStrLn "creating init network"
    -- Training Loop
    putStrLn "starting Training"
    network <- logiter epochs (train eta trainData) initNetwork
    saveNetwork ("network-" ++ show eta ++ "-" ++ show epochs ++ "-" ++ show seed) network

-- Util

logiter :: (Show a, NFData a) => Int -> (a -> a) -> a -> IO a
logiter iter f x
  | iter >= 0 = do
        let y = f x
        putStrLn $ "iter " ++ show iter
        deepseq y $ if iter == 0 then return y else logiter (iter - 1) f y
  | otherwise = error "no negative iter!"
