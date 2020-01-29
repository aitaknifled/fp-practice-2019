module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ x [] = x
foldl f x (head : list) = foldl f (f x head) list

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ x [] = x
foldr f x (head : list) = f head (foldr f x list)

-- В стандартной библиотеке живёт тип по имени Maybe:
-- data Maybe a = Nothing | Just a
-- foldr сворачивает list справа налево => unflodr разворачивает с первого элемента
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x =
  case f x of
    Just (head, new_b) -> head : (unfoldr f new_b)
    Nothing -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f list = reverse (foldl (func f) [] list)
  where
    func f x head = (f head) : x

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product list = foldl (*) 1 list

-- Выделение из списка Maybe всех существующих значений
-- (легче было бы не использовать foldr...)
catMaybes :: [Maybe a] -> [a]
catMaybes list = foldr func [] list
  where
    func tail x =
      case tail of
        Just a -> a : x
        Nothing -> x

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal m = reverse (foldl (func 0) [] m)
  where
    func counter x (head : list)
      | counter >= (count 0 x) = head : x
      | counter < (count 0 x) = func (counter + 1) x list
    count counter [] = counter
    count counter (head : tail) = count (counter + 1) tail

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p list = foldr (func p) [] list
  where
    func p tail x
      | p tail == False = tail : x
      | p tail == True = x

-- Поиск элемента в списке
-- с использованием map
elem :: (Eq a) => a -> [a] -> Bool
elem e list = foldl (||) False (map ((==) e) list)

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = [from, from + step .. to - 1]

-- Конкатенация двух списков
-- (надоело пытаться везде использовать foldl / foldr / reverse)
append :: [a] -> [a] -> [a]
append [] list2 = list2
append (head : list1) list2 = head : append list1 list2

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups list n  = func3 (func1 list n) (func2 list n) n

func1 [] _ = []
func1 _ 0 = []
func1 (head : list) n = head : func1 list (n - 1)

func2 [] _ = []
func2 list 0 = list
func2 (head : list) n = func2 list (n - 1)

func3 list1 [] n = list1 : []
func3 list1 list2 n = list1 : func3 (func1 list2 n) (func2 list2 n) n

