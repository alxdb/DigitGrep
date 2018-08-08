module AI.NeuralNet (
generateNeuralNet,
saveNetwork,
loadNetwork,
train
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
-- a tuple containing the gradient of the weights, and biases in a network
type Gradient = [Nabla]
-- |Nabla
-- the gradient for an individual layer
type Nabla = (Weights, Biases)

type Weights = Matrix R
type Biases = Vector R

-- |Randomly generate starting values for neural network
generateNeuralNet :: Int -> [Int] -> NeuralNet
generateNeuralNet seed sizes = map genRandLayer dims
    where
        genRandLayer (input, output) = (genRandWeights output input, genRandBiases output)
        genRandWeights rows cols = rows><cols $ randoms g
        genRandBiases size = size |> randoms g
        g = mkStdGen seed
        dims = zip sizes (tail sizes)


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
feedBack :: Gradient -> (Vector R, Vector R, Weights) -> Gradient
feedBack nabla (z, a, w)  = nabla' : nabla
    where
        nabla' = (nabla_w, nabla_b)
        nabla_w = asColumn nabla_b * asRow a
        nabla_b = tr w #> (snd . head $ nabla) * cmap sigmoid' z

-- |Backpropagation algorithm
-- performs backpropagation on a network, and a training pair using feedFore and feedBack
backProp :: NeuralNet -> (Vector R, Vector R) -> Gradient
backProp network (input, output) = foldl feedBack [(asColumn initDel * asRow initAct, initDel)] backSet
    where
        initDel = ((head . fst $ foreRes) - output) * cmap sigmoid' (head . snd $ foreRes)
        initAct = fst foreRes !! 1
        foreRes = foldl feedFore ([input], []) network
        backSet = map (\i -> (snd foreRes !! i, fst foreRes !! (i + 1), fst (reverse network !! (i - 1)))) [1..length network - 1]

-- |Training Function
-- applys back propagation to a network given a set of input data
train :: NeuralNet -> R -> [(Vector R, Vector R)] -> NeuralNet
train network eta ioPairs = mapPair applyDel (network, grad)
    where
        applyDel :: Layer -> Nabla -> Layer
        applyDel (w, b) (dw, db) = (w - scalar (eta / fromIntegral (length ioPairs)) * dw, b - scalar (eta / fromIntegral (length ioPairs)) * db)
        grad = foldl (zipWith addPair) grad_init del_grads
        grad_init = map (\l -> (konst 0 . size . fst $ l, konst 0 . size . snd $ l)) network
        del_grads = map (backProp network) ioPairs

-- Util

sigmoid :: R -> R
sigmoid z = 1.0 / (1.0 + exp (-z))

sigmoid' :: R -> R
sigmoid' z = sigmoid z * (1 - sigmoid z)

mapPair :: (a -> b -> c) -> ([a], [b]) -> [c]
mapPair _ ([], [])     = []
mapPair f (x:xs, y:ys) = f x y : mapPair f (xs, ys)

addPair :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addPair (xa, xb) (ya, yb) = (xa + ya, xb + yb)
