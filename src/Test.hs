module Main where

import Data.IDX
import qualified Data.Vector.Unboxed as V

fromJust :: Maybe a -> a
fromJust Nothing  = error "found Nothing"
fromJust (Just x) = x

main :: IO ()
main = do
    putStrLn "loading data"
    trainImages <- decodeIDXFile "data/test-images.idx"
    trainLabels <- decodeIDXLabelsFile "data/test-labels.idx"
    putStrLn "creating dataStructures"
    let trainData = fromJust $ labeledDoubleData (fromJust trainLabels) (fromJust trainImages)
    let trainDims = V.toList $ idxDimensions (fromJust trainImages)
    putStrLn "testing"
    putStrLn "Done"
