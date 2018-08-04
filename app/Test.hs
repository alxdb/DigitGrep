module Main where

import           AI.NeuralNet
import           Control.Monad (replicateM)

fromJust :: Maybe a -> a
fromJust Nothing  = error "found Nothing"
fromJust (Just x) = x

(>?) :: (a -> b) -> Maybe a -> Maybe b
(>?) f (Just x) = Just (f x)
(>?) _ Nothing  = Nothing

(>>?) :: Maybe (a -> b) -> Maybe a -> Maybe b
(>>?) (Just f) (Just x) = Just (f x)
(>>?) _ Nothing         = Nothing
(>>?) Nothing _         = Nothing

main :: IO ()
main = do
    -- putStrLn "loading data"
    -- trainingData <- getTrainingData
    -- putStrLn "loading network"
    -- network <- readFile "network" >>= readIO :: IO (NetworkData Double)
    -- print "done"
    let network = generateNeuralNet 123 [738, 16, 16, 10]
    saveNetwork "network" network
    print "done"
