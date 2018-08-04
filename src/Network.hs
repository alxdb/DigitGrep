module Network where

import           Control.Monad       (replicateM)
import           Data.Binary
import           Data.IDX
import qualified Data.Vector.Unboxed as VB
import           Linalg
import           System.Random

newtype WeightData a = WeightData [[a]] deriving (Show, Read)
newtype BiasData a = BiasData [a] deriving (Show, Read)

data LayerData a = LayerData {
    weights :: WeightData a,
    biases  :: BiasData a
} deriving (Show, Read)

newtype NetworkData a = NetworkData {layers :: [LayerData a]} deriving (Show, Read)

fromJust :: Maybe a -> a
fromJust Nothing  = error "found Nothing"
fromJust (Just x) = x

(>?) :: (a -> b) -> Maybe a -> b
(>?) f (Just x) = f x
(>?) _ Nothing  = error "found nothing"

sigmoid :: (Floating a) => a -> a
sigmoid x = 1 / (1 + exp (-x))

propagate :: (Floating a) => [a] -> LayerData a -> [a]
propagate input (LayerData (WeightData weights) (BiasData biases)) = map sigmoid $ (weights ||*| input) |+| biases

cost :: (Floating a) => [a] -> Int -> a
cost result expected = sum $ map (** 2) (result |-| [ if x == expected then 1 else 0 | x <- [0 .. 9] ])

runNetwork :: (Floating a) => [a] -> NetworkData a -> [a]
runNetwork input (NetworkData layers) = foldl propagate input layers

initialiseLayer :: (Num a, Random a) => Int -> Int -> IO (LayerData a)
initialiseLayer inputSize layerSize = do
    w <- replicateM layerSize (replicateM inputSize (randomRIO (-1, 1)))
    b <- replicateM layerSize (randomRIO (-1, 1))
    return LayerData {weights = WeightData w, biases = BiasData b}

createConnectionTuples :: [Int] -> [(Int, Int)]
createConnectionTuples sizes = map (\i -> (sizes!!i, sizes!!(i + 1))) [0..length sizes - 2]

initialiseNetwork :: (Num a, Random a) => [Int] -> Int -> IO (NetworkData a)
initialiseNetwork layerSizes seed = do
    setStdGen (mkStdGen seed)
    randomLayers <- sequence [ initialiseLayer from to | (from, to) <- createConnectionTuples layerSizes ]
    return NetworkData {layers = randomLayers}

getTrainingData :: IO [(Int, [Double])]
getTrainingData = do
    trainImages <- decodeIDXFile "data/train-images.idx"
    trainLabels <- decodeIDXLabelsFile "data/train-labels.idx"
    return [ (label, VB.toList image) | (label, image) <- fromJust $ ((labeledDoubleData >? trainLabels) >? trainImages)]
