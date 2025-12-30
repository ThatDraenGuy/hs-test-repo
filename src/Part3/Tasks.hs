module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums =
    fst $ maximumBy $ map (\item -> (item, length (filter (== item) digits))) digits
    where
        splitIntoDigits :: Int -> [Int]
        splitIntoDigits 0 = []
        splitIntoDigits num = num `mod` 10 : splitIntoDigits (num `div` 10)

        digits = concat (map splitIntoDigits nums)

        maximumBy' a b = if snd a > snd b then a else b
        maximumBy (head : tail) = foldl maximumBy' head tail


-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq lst =
    foldl (\acc item -> if item `elem` acc then acc else item : acc) [] lst

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst =
    let pairs = map (\item -> (f item, item)) lst in
    foldl collect [] pairs
    where 
        hasKey :: (Eq k) => [(k, [a])] -> k -> Bool
        hasKey acc key = not $ null (filter (\item -> fst item == key) acc)

        insert :: (Eq k) => [(k, [a])] -> (k, a) -> [(k, [a])]
        insert acc pair = map (\item ->
            if fst item == fst pair then 
                (fst item, snd pair : snd item) 
            else item
            ) acc

        collect :: (Eq k) => [(k, [a])] -> (k, a) -> [(k, [a])]
        collect acc pair
            | hasKey acc (fst pair) = insert acc pair
            | otherwise = (fst pair, [snd pair]) : acc
