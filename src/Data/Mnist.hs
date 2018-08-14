module Data.Mnist (getTrainingData, getTestingData) where

import           Data.IDX
import           Data.Vector.Unboxed   as VB
import           Numeric.LinearAlgebra as HM

getTrainingData :: IO [(HM.Vector R, HM.Vector R)]
getTrainingData = getMnistData "data/train-images.idx" "data/train-labels.idx"

getTestingData :: IO [(HM.Vector R, HM.Vector R)]
getTestingData = getMnistData "data/test-images.idx" "data/test-labels.idx"

getMnistData :: FilePath -> FilePath -> IO [(HM.Vector R, HM.Vector R)]
getMnistData images labels= do
    imageData <- decodeIDXFile images
    labelData <- decodeIDXLabelsFile labels
    return [ (vector $ VB.toList (VB.map (/ 255) image), vector [if x == label then 1.0 else 0.0 | x <- [0..9]]) | (label, image) <- fromJust ((labeledDoubleData >? labelData) >? imageData)]

-- Utils

fromJust :: Maybe a -> a
fromJust Nothing  = error "found Nothing"
fromJust (Just x) = x

(>?) :: (a -> b) -> Maybe a -> b
(>?) f x = f (fromJust x)
