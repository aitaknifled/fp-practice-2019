module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

--fac :: Integer -> Integer
fac 0 = 1
fac n =
  if n > 0 then n * fac (n - 1) else -1

pow :: Double -> Integer -> Double
pow x 0 = 1
pow x n = x * pow x (n - 1)


--синус числа (формула Тейлора)
sinn :: Double -> Double
sinn x = summ x 0
  where
  summ :: Double -> Integer -> Double
  summ x 10 = 0
  summ x n = ((-1) ^ n * x ^ (2 * n + 1) / fromIntegral (fac (2 * n + 1))) + summ x (n + 1)
  
-- косинус числа (формула Тейлора)
coss :: Double -> Double
coss x = summ x 0
  where
  summ :: Double -> Integer -> Double
  summ x 10 = 0
  summ x n = ((-1) ^ n * x ^ (2 * n) / fromIntegral (fac (2 * n))) + summ x (n + 1)
  
-- наибольший общий делитель двух чисел
gcdd :: Integer -> Integer -> Integer
gcdd x y = func x y 2 1
  where 
  func :: Integer -> Integer -> Integer -> Integer -> Integer
  func 1 y n nod = nod
  func x 1 n nod = nod
  func x y n nod = 
    if (x `mod` n) == 0 && (y `mod` n) == 0 then func (x `div` n) (y `div` n) n (nod * n) 
      else if (x `mod` n) == 0 then func (x `div` n) y n nod
             else if (y `mod` n) == 0 then func x (y `div` n) n nod
                    else func x y (n + 1) nod

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = func x 2
  where 
  func :: Integer -> Integer -> Bool
  func x n = 
    if x == n then True
      else if (x `mod` n) /= 0 then func x (n + 1)
             else False
