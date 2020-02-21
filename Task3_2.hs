module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

-- для какой-то версии ghci нужно...
import Data.Semigroup
import Data.Monoid
-- (<>) должна удовлетворять x <> (y <> z) = (x <> y) <> z
instance Semigroup (ReverseList a) where
  (<>) a b = mappend a b

data ReverseList a = RNil | RCons (ReverseList a) a


rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons list x) = x : rlistToList list

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList (head : list) = RCons (listToRList list) head

instance (Show a) => Show (ReverseList a) where
  show = show.rlistToList
-- Слева от символа => находится ограничение, наложенное на полиморфный тип a.
-- Мы говорим: «Да, мы готовы принять аргумент любого типа,
-- но при условии, что этот тип относится к классу типов Eq».
instance (Eq a) => Eq (ReverseList a) where
  (==) RNil RNil = True
  (==) (RCons a b) (RCons a1 b1) = a == a1 && b == b1
  (==) _ _ = False


instance (Ord a) => Ord (ReverseList a) where
  (<=) a b = rlistToList a <= rlistToList b 

instance Monoid (ReverseList a) where
  mempty = RNil
  mappend x RNil = x
  mappend RNil y = y
  mappend x (RCons y v) = RCons (mappend x y) v


--class Functor f where
--fmap :: (a -> b) -> f a -> f b
instance Functor ReverseList where
  --fmap _ RNil = RNil
  --fmap f (RCons xs x) = RCons (fmap f xs) (f x)
  fmap func x = listToRList (map func (rlistToList x))

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor