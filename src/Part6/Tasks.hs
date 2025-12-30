{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map hiding (foldl, map, splitAt)
import Data.Maybe

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       -- width -> height -> (\x y -> elem) -> result (matrix filled by func)
       mfill :: Int -> Int -> (Int -> Int -> Int) -> mx
       -- matrix -> result (matrix width)
       mwidth :: mx -> Int
       -- matrix -> result (matrix height)
       mheight :: mx -> Int
       -- matrix -> x -> y -> result (value at x y)
       mget :: mx -> Int -> Int -> Int
       -- matrix -> result (matrix without first row)
       mtrimRow :: mx -> mx
       -- matrix -> column index -> result (matrix without column at index)
       mdropCol :: mx -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       mfill 1 1 f = f 0 0
       mwidth _ = 1
       mheight _ = 1
       mget m _ _ = m
       mtrimRow m = m
       mdropCol m _ = m

instance Matrix [[Int]] where
       mfill w h f = (\y -> (`f` y) <$> [0..(w - 1)]) <$> [0..(h - 1)]
       mwidth m = length (head m)
       mheight m = length m
       mget m x y = m !! y !! x
       mtrimRow (head : tail) = tail
       mdropCol m i = map (deleteAt i) m
              where
                     deleteAt idx lst = lft ++ rgt
                            where (lft, (_:rgt)) = splitAt idx lst

instance Matrix (SparseMatrix Int) where
       mfill w h f = SparseMatrix w h (fromList values)
              where coords = [0..(h - 1)] >>= (\y -> fmap (\x -> (x, y)) [0..(w - 1)])
                    values = (coords >>= (\pair -> if f (fst pair) (snd pair) /= 0 then [(pair, f (fst pair) (snd pair))] else []))
       mwidth m = sparseMatrixWidth m
       mheight m = sparseMatrixHeight m
       mget m x y = case (Data.Map.lookup (x, y) (sparseMatrixElements m)) of
              Just num -> num
              Nothing -> 0
       mtrimRow m = mfill (mwidth m) (mheight m - 1) (\x y -> mget m x (y + 1))
       mdropCol m i = mfill (mwidth m - 1) (mheight m) getElem
              where getElem x y
                     | x < i = mget m x y
                     | otherwise = mget m (x + 1) y

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = mfill w w (\x y -> if x == y then 1 else 0)
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = mfill w h (\x y -> 0)

createMatrix :: Matrix m => [[Int]] -> m
createMatrix lst = mfill (length (head lst)) (length lst) (\x y -> lst !! y !! x)


-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b
       | mwidth a /= mheight b = error "Cannot multiply non matching height+width"
       | otherwise = mfill (mheight a) (mwidth b) calcElem
       where
              count = mwidth a
              calcElem x y = foldl (\acc index -> acc + (mget a index y) * (mget b x index)) 0 [0..(count - 1)]

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m
       | mwidth m /= mheight m = error "Cannot calculate determinant of non-square matrix"
       | otherwise = foldl (\acc i -> acc + calcDetTerm i) 0 [0..(count - 1)]
       where 
              count = mwidth m
              calcDetTerm :: Int -> Int
              calcDetTerm i
                     | count == 1 = mget m 0 0
                     | otherwise = sign * baseElem * determinant subMatrix
                     where
                            sign = (-1) ^ i
                            baseElem = mget m i 0
                            subMatrix = mdropCol (mtrimRow m) i