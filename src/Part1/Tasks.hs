module Part1.Tasks where

import Util(notImplementedYet)

fact :: Integer -> Integer
fact i = if i <= 1 then 1 else i * fact (i - 1)

taylorAtZero x stepFunc currSum n eps =
    let sum = stepFunc n x in
    if abs sum < eps
    then currSum 
    else taylorAtZero x stepFunc (currSum + sum) (n + 1) eps

sinStepFunc n x = 
    let num = 2 * n + 1
        sign = if (mod n 2) == 0 then 1 else -1
    in sign * x ** (fromInteger num) / (fromInteger . fact $ num)

mySin' x = taylorAtZero x sinStepFunc 0 0 0.0001

cosStepFunc n x =
    let num = 2 * n
        sign = if (mod n 2) == 0 then 1 else -1
    in sign * x ** (fromInteger num) / (fromInteger . fact $ num)

myCos' x = taylorAtZero x cosStepFunc 0 0 0.0001

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = let adjusted = x - 2 * pi * (fromInteger . floor $ (x / (2 * pi))) in
    mySin' adjusted

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = let adjusted = x - 2 * pi * (fromInteger . floor $ (x / (2 * pi))) in
    myCos' adjusted

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = gcd (abs b) (mod (abs a) (abs b))

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y = notImplementedYet

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow a 1 = a
myPow a b = a * myPow a (b - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime = notImplementedYet

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
