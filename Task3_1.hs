module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа
instance Show WeirdPeanoNumber where
  show Zero = "Zero"
  show (Succ wpn) = "Succ " ++ show wpn
  show (Pred wpn) = "Pred " ++ show wpn

instance Enum WeirdPeanoNumber where
  --toEnum :: Int -> WeirdPeanoNumber
  toEnum 0 = Zero
  toEnum i
    |i > 0 = Succ(toEnum (i - 1))
    |i < 0 = Pred(toEnum (i + 1))

  --fromEnum :: WeirdPeanoNumber -> Int
  fromEnum Zero = 0
  fromEnum (Succ a) = fromEnum a + 1
  fromEnum (Pred a) = fromEnum a - 1
instance Num WeirdPeanoNumber where
  w1 + w2 = toEnum (fromEnum w1 + fromEnum w2)
  w1 - w2 = toEnum (fromEnum w1 - fromEnum w2)
  w1 * w2 = toEnum (fromEnum w1 * fromEnum w2)
  abs = toEnum.abs.fromEnum
  signum = toEnum.signum.fromEnum

  --fromInteger :: Integer -> WeirdPeanoNumber
  fromInteger 0 = Zero
  fromInteger i
    |i > 0 = Succ(fromInteger (i - 1))
    |i < 0 = Pred(fromInteger (i + 1))


instance Eq WeirdPeanoNumber where
  wpn1 == wpn2 = fromEnum wpn1 == fromEnum wpn2
  
instance Ord WeirdPeanoNumber where
  wpn1 <= wpn2 = fromEnum wpn1 <= fromEnum wpn2
  
instance Real WeirdPeanoNumber where
  --toRational :: WeirdPeanoNumber -> Rational
  toRational = toRational.fromEnum
  
instance Integral WeirdPeanoNumber where
  --toInteger :: WeirdPeanoNumber -> Integer
  toInteger = toInteger.fromEnum
  --quotRem :: a -> a -> (a, a) 
  quotRem a b = ((toEnum (fst (f a b))),(toEnum (snd (f a b))))
    where
      f a b = quotRem (fromEnum a) (fromEnum b)

