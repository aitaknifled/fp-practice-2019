module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
import Control.Applicative
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
  fmap f (FunMonad func) = FunMonad(\x -> f (func x))

instance Applicative FunMonad where
  --(<*>) :: f (String -> a -> b) -> f (String -> a) -> f (String -> b)
  (FunMonad funFunc) <*> (FunMonad funA) = FunMonad(\a -> funFunc a (funA a))
  --pure :: a -> f a
  pure a = FunMonad(\x -> a)

-- Законы монад:
-- return a >>= k = k a
-- m >>= return = m
-- m >>= k >>= k' = m >>= (\x -> k x >>= k')

instance Monad FunMonad where
  return = pure
  --(>>=) :: forall a b. m a -> (a -> m b) -> m b
  --(>>=) :: f (String -> a) -> ((String -> a) -> f (String -> b)) -> f (String -> b)
  --fun функция взятия из FunMonad значения аргумента fun
  (>>=) (FunMonad fm) f = FunMonad(\x -> fun (f (fm x)) x)
