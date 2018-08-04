module Linalg where

(|+|) :: (Num a) => [a] -> [a] -> [a]
x |+| y = zipWith (+) x y

(|-|) :: (Num a) => [a] -> [a] -> [a]
x |-| y = zipWith (-) x y

(|*|) :: (Num a) => [a] -> [a] -> a
x |*| y = sum $ zipWith (*) x y

(||*|) :: (Num a) => [[a]] -> [a] -> [a]
x ||*| y = map (|*| y) x
