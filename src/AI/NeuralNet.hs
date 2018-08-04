module AI.NeuralNet (
generateNeuralNet,
saveNetwork,
loadNetwork
) where

import           Data.Binary
import           Numeric.LinearAlgebra
import           System.Random

-- |Neural Network
-- a list of Layers describing the network
type NeuralNet = [Layer]
-- |Network Layer
-- a tuple of the weights connecting the previous layer to this one, and the biases of this layer
type Layer = (Weights, Biases)
-- |Gradient
-- a tuple containing the gradient of the weights, and biases in a layer
type Nabla = (Matrix R, Vector R)

type Weights = Matrix R
type Biases = Vector R

-- |Randomly generate starting values for neural network
generateNeuralNet :: Int -> [Int] -> NeuralNet
generateNeuralNet seed sizes = map genRandLayer dims
    where
        g = mkStdGen seed
        dims = zip sizes (tail sizes)
        genRandWeights rows cols = rows><cols $ randoms g
        genRandBiases size = size |> randoms g
        genRandLayer (input, output) = (genRandWeights output input, genRandBiases output)


saveNetwork :: FilePath -> NeuralNet -> IO ()
saveNetwork = encodeFile

loadNetwork :: FilePath -> IO NeuralNet
loadNetwork = decodeFile

-- Neural Network Logic

iterateForward :: Layer -> Vector R -> Vector R
iterateForward (w, b) input = (w #> input) + b

-- |Feedforward algorithm for Backpropagation
-- performs normal feed forward but stores intermediate values that will be needed in backpropagation
feedFore :: ([Vector R], [Vector R]) -> Layer -> ([Vector R], [Vector R])
feedFore (a, z) layer = (a' : a, z' : z)
    where
        z' = iterateForward layer (head a)
        a' = cmap sigmoid z'

-- |Feedback algorithm for BackPropagation
-- takes a list of nablas, the current zs, the backwards activations and the forward weights
feedBack :: [Nabla] -> (Vector R, Vector R, Weights) -> [Nabla]
feedBack nabla (z, a, w)  = nabla' : nabla
    where
        nabla' = (nabla_w, nabla_b)
        nabla_w = asColumn nabla_b * asRow a
        nabla_b = tr w #> (snd . head $ nabla) * cmap sigmoid' z

-- |Backpropagation algorithm
-- performs backpropagation on a network, and a training pair using feedFore and feedBack
backProp :: NeuralNet -> (Vector R, Vector R) -> [Nabla]
backProp network (input, output) = foldl feedBack [(asColumn initDel * asRow initAct, initDel)] backSet
    where
        foreRes = foldl feedFore ([input], []) network
        initDel = ((head . fst $ foreRes) - output) * cmap sigmoid' (head . snd $ foreRes)
        initAct = fst foreRes !! 1
        backSet = map (\i -> (snd foreRes !! i, fst foreRes !! (i + 1), fst (reverse network !! (i - 1)))) [1..length network - 1]

-- Util

sigmoid :: R -> R
sigmoid z = 1.0 / (1.0 + exp (-z))

sigmoid' :: R -> R
sigmoid' z = sigmoid z * (1 - sigmoid z)
