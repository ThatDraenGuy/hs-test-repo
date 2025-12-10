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
isDateCorrect d m y = 
    let isLeapYear = (mod y 4 == 0) && (mod y 100 /= 0 || mod y 400 == 0)
        upperDayBound month
            | elem month [1, 3, 5, 7, 8, 10, 12] = 31
            | elem month [4, 6, 9, 11] = 30
            | otherwise = if isLeapYear then 29 else 28
    in
    d > 0 && d <= upperDayBound m && m > 0 && m <= 12 && y > 0

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow a 1 = a
myPow a b = a * myPow a (b - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime
    num
        | num <= 1 = False
        | num == 2 = True
        | otherwise = all (num `notDivisible`) [2..(num `div` 2)]
    where notDivisible x y = (mod x y) /= 0


type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea [] = 0.0
shapeArea points =
    let calcPair (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
        pairWindow arr func = zipWith func arr (tail arr ++ [head arr]) in
    abs (0.5 * sum (pairWindow points calcPair))


-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c =
    let (big, x, y) = if a > b && a > c then (a, b, c) else if b > c then (b, a, c) else (c, a,b) 
        bigSqr = big ** 2
        sidesSqr = x ** 2 + y ** 2
    in
    if a + b <= c || b + c <= a || a + c <= b then -1
    else if bigSqr > sidesSqr then 0
    else if bigSqr < sidesSqr then 1
    else 2
    
