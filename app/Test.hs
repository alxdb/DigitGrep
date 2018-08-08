module Main where

import           AI.NeuralNet
import           Data.Mnist

main :: IO ()
main = do
    let network = loadNetwork "network"
    print "done"
