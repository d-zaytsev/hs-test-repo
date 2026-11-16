module Part1.Tasks where

import Data.Fixed (mod')
import Util (notImplementedYet)

sumSquares :: Int -> Int -> Int
sumSquares x y = square x + square y
  where
    square :: Int -> Int
    square n = n * n

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = helper 0 1 z
  where
    eps = 0.000001
    z = ((mod' (x + pi) (2 * pi)) - pi)
    helper :: Double -> Double -> Double -> Double
    helper s n y = if abs y > eps then helper (s + y) (n + 1) (-y * z * z / ((2 * n) * (2 * n + 1))) else s

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = helper 0 1 1
  where
    eps = 0.000001
    z = (mod' (x + pi) (2 * pi)) - pi
    helper :: Double -> Double -> Double -> Double
    helper s n y = if abs y > eps then helper (s + y) (n + 1) (-y * z * z / ((2 * n - 1) * (2 * n))) else s

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (mod a b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y
  | d < 0 || d > 31 = False
  | m < 0 || m > 12 = False
  | y < 0 = False
  | otherwise = d <= maxDay
  where
    maxDay
      | m == 4 || m == 6 || m == 9 || m == 11 = 30
      | m == 2 = if (mod y 4 == 0 && mod y 100 /= 0) || mod y 400 == 0 then 29 else 28
      | otherwise = 31

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x = helper 1
  where
    helper :: Integer -> Integer -> Integer
    helper acc 0 = acc
    helper acc n = helper (acc * x) (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = filter (\y -> mod x y == 0) [2 .. floor (sqrt (fromIntegral x))] == []

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea [] = 0
shapeArea points@(first@(firstX, firstY) : rest) = abs (l - r) / 2
  where
    (xs, ys) = unzip points

    lzip = zip xs (drop 1 ys)
    l = sum (map (\(x, y) -> x * y) lzip) + last xs * firstY

    rzip = zip (drop 1 xs) ys
    r = sum (map (\(x, y) -> x * y) rzip) + firstX * last ys

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | a > b = triangleKind b a c
  | b > c = triangleKind a c b
  | a > c = triangleKind c b a
  | otherwise = check a b c
  where
    check a b c
      | a + b <= c = -1
      | c ** 2 > a ** 2 + b ** 2 = 0
      | c ** 2 < a ** 2 + b ** 2 = 1
      | otherwise = 2
