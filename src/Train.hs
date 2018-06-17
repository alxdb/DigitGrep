module Main where

import Control.Monad (replicateM)
import Data.IDX
import qualified Data.Vector.Unboxed as V
import Data.List
import Network
import System.Random

fromJust :: Maybe a -> a
fromJust Nothing  = error "found Nothing"
fromJust (Just x) = x

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

seed :: Int
seed = 1234

main :: IO ()
main = do
    -- Initialization
    putStrLn "loading data"
    trainImages <- decodeIDXFile "data/train-images.idx"
    trainLabels <- decodeIDXLabelsFile "data/train-labels.idx"
    putStrLn "creating dataStructures"
    let trainData = fromJust $ labeledDoubleData (fromJust trainLabels) (fromJust trainImages)
    let trainDims = V.toList $ idxDimensions (fromJust trainImages)
    let inputSize = (trainDims !! 1) * (trainDims !! 2)
    putStrLn "initializing network"
    setStdGen (mkStdGen seed)
    let layerSizes = [inputSize, 16, 16, 10]
    network <- initialiseNetwork layerSizes :: IO (NetworkData Double)
    -- Training Loop
    putStrLn "training"
    costAvg <- mapM(\limit -> 
        average <$> mapM (\sample -> do
            let input = V.toList $ snd (trainData !! sample)
            let expected = fst (trainData !! sample)
            let output = runNetwork input network
            putStr $ "predicted " ++ show (fromJust $ elemIndex (foldl max 0.0 output) output)
                ++ " | expected " ++ show expected
                ++ " | cost " ++ show (cost output expected)
                ++ "\r"
            return $ cost output expected
        ) [0..limit]
        ) [0..10]
    putStrLn "Done"
