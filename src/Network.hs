module Network where

(+|) :: (Num a) => [a] -> [a] -> [a]
x +| y = zipWith (+) x y

(*|) :: (Num a) => [a] -> [a] -> a
x *| y = sum $ zipWith (*) x y

(|*|) :: (Num a) => [[a]] -> [a] -> [a]
x |*| y = map (*| y) x

propagate :: (Num a) => [[a]] -> [a] -> [a] -> [a]
propagate weights input biases = weights |*| input +| biases
