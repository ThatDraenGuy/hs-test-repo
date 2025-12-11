module Part3.Tasks where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)


splitIntoDigits :: Int -> [Int]
splitIntoDigits 0 = []
splitIntoDigits num = num `mod` 10 : splitIntoDigits (num `div` 10)
-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums =
    let digits = concat (map splitIntoDigits nums) in
    fst $ maximumBy (comparing snd) $ map (\item -> (item, length (filter (== item) digits))) digits



firstIndexOf' :: (Eq a) => [a] -> a -> Int -> Int
firstIndexOf' lst item idx = if head lst == item then idx else firstIndexOf' (tail lst) item idx + 1

firstIndexOf :: (Eq a) => [a] -> a -> Int
firstIndexOf lst item = firstIndexOf' lst item 0

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq lst = map snd $ filter (\item -> fst item == firstIndexOf lst (snd item)) (zip [0..] lst)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.

hasKey :: (Eq k) => [(k, [a])] -> k -> Bool
hasKey acc key = not $ null (filter (\item -> fst item == key) acc)

insert :: (Eq k) => [(k, [a])] -> (k, a) -> [(k, [a])]
insert acc pair = map (\item -> if fst item == fst pair then (fst item, snd pair : snd item) else item) acc

grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst =
    let pairs = map (\item -> (f item, item)) lst in
    foldl collect [] pairs
    where collect acc pair = if hasKey acc (fst pair) then insert acc pair else (fst pair, [snd pair]) : acc
