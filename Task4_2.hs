module Task4_2 where


{-
  Задание 4.2
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}
import Control.Applicative
data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

-- do { e1 ; e2 } = e1 >> e2
-- do { p <- e1; e2 } = e1 >>= \p -> e2
-- do { let v = e; e2 } = let v = e in do e2

-- (>>) :: m a -> m b -> m b

instance Functor FourOf where
  fmap f (FourOf a1 a2 a3 a4) = FourOf (f a1) (f a2) (f a3) (f a4)

instance Applicative FourOf where
  --pure :: a -> f a
  pure a = FourOf a a a a
  --(<*>) :: f (a -> b) -> f a -> f b
  (<*>) (FourOf f1 f2 f3 f4) (FourOf a1 a2 a3 a4) = FourOf (f1 a1) (f2 a2) (f3 a3) (f4 a4)

instance Monad FourOf where
  return = pure 
  --(>>=) :: forall a b. m a -> (a -> m b) -> m b
  (>>=) (FourOf a1 a2 a3 a4) f = FourOf (f1 (f a1)) (f2 (f a2)) (f3 (f a3)) (f4 (f a4))
    where
      f1 (FourOf b1 _ _ _) = b1
      f2 (FourOf _ b2 _ _) = b2
      f3 (FourOf _ _ b3 _) = b3
      f4 (FourOf _ _ _ b4) = b4
