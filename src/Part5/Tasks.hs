module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (h:t) = myFoldl f (f acc h) t

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (h:t) = f h (myFoldr f acc t)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldl (\acc -> \item -> acc ++ [f item]) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldl (\acc -> \item -> acc ++ (f item)) []

myConcat :: [[a]] -> [a]
myConcat = myConcatMap id

myReverse :: [a] -> [a]
myReverse = myFoldr (\item -> \acc -> acc ++ [item]) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldl (\acc -> \item -> if p item then acc ++ [item] else acc ) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldl (\acc -> \item -> if p item then (fst acc ++ [item], snd acc) else (fst acc, snd acc ++ [item])) ([], [])

