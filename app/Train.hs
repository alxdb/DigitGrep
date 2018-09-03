module Main where

import           AI.NeuralNet
import           Control.DeepSeq
import           Control.Monad
import           Data.Mnist
import           Data.List.Split
import           Numeric.LinearAlgebra

seed :: Int
seed = 123

eta :: R
eta = 3.0

hiddenLayers :: [Int]
hiddenLayers = [10]

epochs :: Int
epochs = 30

subsetsize :: Int
subsetsize = 10

main :: IO ()
main = do
    -- Initialization
    trainData <- getTrainingData
    let layerSizes = [size . fst . head $ trainData] ++ hiddenLayers ++ [size . snd . head $ trainData]
    let initNetwork = generateNeuralNet seed layerSizes
    putStrLn "creating init network"
    -- Training Loop
    putStrLn "starting Training"
    network <- logiter epochs (\ iternet -> foldl (\ net dat -> train eta dat net) iternet (chunksOf subsetsize trainData)) initNetwork
    saveNetwork ("network-" ++ show eta ++ "-" ++ show epochs ++ "-" ++ show seed) network

trainingLoop :: NeuralNet -> [(Vector R, Vector R)] -> IO NeuralNet
trainingLoop net dat = do
  network <- logiter epochs (train eta dat) net
  return network

-- Util

logiter :: (Show a, NFData a) => Int -> (a -> a) -> a -> IO a
logiter iter f x
  | iter >= 0 = do
        let y = f x
        putStrLn $ "iter " ++ show iter
        deepseq y $ if iter == 0 then return y else logiter (iter - 1) f y
  | otherwise = error "no negative iter!"
