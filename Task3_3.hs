module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}
import Data.Monoid
--Тип, определяемый с помощью слова newtype, обязан иметь один и только один кон-
--структор значения. Кроме того, в таком типе должно быть одно и лишь одно поле.

import Data.Monoid
-- (<>) должна удовлетворять x <> (y <> z) = (x <> y) <> z
instance Semigroup (PSet a) where
  (<>) a b = mappend a b
instance Semigroup (PSet2 a) where
  (<>) a b = mappend a b
  
newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Содержится ли искомое хоть в одном из множеств

instance Monoid (PSet a) where
  mempty = PSet (\x -> False)
  mappend (PSet x) (PSet y) = PSet (\a -> x a || y a)


newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }

-- Содержится ли искомое в каждом из множеств

instance Monoid (PSet2 a) where
  mempty = PSet2 (\x -> False)
  mappend (PSet2 x) (PSet2 y) = PSet2 (\a -> x a && y a)


-- Tребуется функция следующего вида: (PSet a -> PSet b)...( (a -> Bool) -> (b -> Bool) )
-- Не имея информации об определении "первой функции (a -> Bool)" невозможно определить такую функцию.

--fmap = оператор <$>
--(<$) :: a -> f b -> f a
instance Functor PSet where
  --fmap f (PSet c) = PSet (f c) -- так нельзя, тк "c" это функциональный тип... 
  --но если бы тип был вида (Bool -> a), то было бы можно
  --fmap f (PSet c) = PSet (\a -> f (c a)) -- так  тоже нельзя(
  fmap _ _ = PSet (\_ -> False)