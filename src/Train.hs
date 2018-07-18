module Main where

import Control.Monad (replicateM, when)
import Data.IDX
import qualified Data.Vector.Unboxed as V
import Data.List
import Network
import System.Random
import Numeric

fromJust :: Maybe a -> a
fromJust Nothing  = error "found Nothing"
fromJust (Just x) = x

formatFloatN :: RealFloat a => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

average :: Fractional a => [a] -> a
-- average xs = sum xs / fromIntegral (length xs) this is very slow
average = f 0 0
    where
        f :: Fractional b => b -> Int -> [b] -> b
        f s l [] = s / fromIntegral l
        f s l (x:xs) = f (s + x) (l + 1) xs

seed :: Int
seed = 1234

-- Manually ensure 60 000 is divisible by this number
trainingSubsets :: Int
trainingSubsets = 12

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
    let subsetSize = div (head trainDims) trainingSubsets
    putStrLn "initializing network"
    setStdGen (mkStdGen seed)
    let layerSizes = [inputSize, 16, 16, 10]
    network <- initialiseNetwork layerSizes :: IO (NetworkData Double)
    -- Training Loop
    putStrLn "training"
    costAvg <- mapM(\iter -> do
        putStrLn $ "calculating average for subset " ++ show (iter + 1) ++ " of " ++ show trainingSubsets
        avg <- do
            costs <- mapM (\sample -> do
                let input = V.toList $ snd (trainData !! sample)
                let expected = fst (trainData !! sample)
                let output = runNetwork input network
                return $ cost output expected
                ) [iter * subsetSize .. ((iter + 1) * subsetSize - 1)]
            return $ average costs
        putStrLn $ "average cost: " ++ formatFloatN avg 2
        return avg
        ) [0..(trainingSubsets - 1)]
    putStrLn "Done"
