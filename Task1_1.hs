module Task1_1 where

{-
  Задание 1.1, 
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

instance Show Term where
    show (IntConstant intValue) = show intValue
    show (Variable varName) = varName
    show (BinaryTerm operation lhv rhv) = 
      case operation of
        (|+|) -> show lhv ++ " + " ++ show rhv

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ operation :: Term -> Term -> Term, lhv :: Term, rhv :: Term } -- бинарная операция
            | Num
            --deriving(Show,Eq)


-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет

(|+|) :: Term -> Term -> Term
(|+|) l r = 
  case l of
    IntConstant intL ->
      case r of 
        IntConstant intR -> IntConstant (intL + intR)
        Variable varName -> Variable (show l ++ " + " ++ show r)
        BinaryTerm operation lhv rhv -> l |+| operation lhv rhv
    Variable varNameL -> 
      case r of
        IntConstant intValue -> Variable (show l ++ " + " ++ show r)
        Variable varNameR -> Variable (show l ++ " + " ++ show r)
        BinaryTerm operation lhv rhv -> l |+| operation lhv rhv
    BinaryTerm operationL lhvL rhvL ->
      case r of
        IntConstant intValue -> operationL lhvL rhvL |+| r
        Variable varName -> operationL lhvL rhvL |+| r
        BinaryTerm operationR lhvR rhvR -> operationL lhvL rhvL |+| r

(|-|) :: Term -> Term -> Term
(|-|) l r = 
  case l of
    IntConstant intL ->
      case r of 
        IntConstant intR -> IntConstant (intL - intR)
        Variable varName -> Variable (show l ++ " + " ++ show r)
        BinaryTerm operation lhv rhv -> l |-| operation lhv rhv
    Variable varNameL -> 
      case r of
        IntConstant intValue -> Variable (show l ++ " + " ++ show r)
        Variable varNameR -> Variable (show l ++ " + " ++ show r)
        BinaryTerm operation lhv rhv -> l |-| operation lhv rhv
    BinaryTerm operationL lhvL rhvL ->
      case r of
        IntConstant intValue -> operationL lhvL rhvL |-| r
        Variable varName -> operationL lhvL rhvL |-| r
        BinaryTerm operationR lhvR rhvR -> operationL lhvL rhvL |-| r

(|*|) :: Term -> Term -> Term
(|*|) l r = 
  case l of
    IntConstant intL ->
      case r of 
        IntConstant intR -> IntConstant (intL * intR)
        Variable varName -> Variable (show l ++ " + " ++ show r)
        BinaryTerm operation lhv rhv -> l |*| operation lhv rhv
    Variable varNameL -> 
      case r of
        IntConstant intValue -> Variable (show l ++ " + " ++ show r)
        Variable varNameR -> Variable (show l ++ " + " ++ show r)
        BinaryTerm operation lhv rhv -> l |*| operation lhv rhv
    BinaryTerm operationL lhvL rhvL ->
      case r of
        IntConstant intValue -> operationL lhvL rhvL |*| r
        Variable varName -> operationL lhvL rhvL |*| r
        BinaryTerm operationR lhvR rhvR -> operationL lhvL rhvL |*| r
-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = 
  case expression of
    IntConstant var -> expression
    Variable var -> 
      if var == varName then replacement else expression
    BinaryTerm operation l r -> expression { lhv = replaceVar varName replacement l, rhv = replaceVar varName replacement r}

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = 
  case expression of
    IntConstant var -> expression
    Variable var -> expression
    BinaryTerm operation l r -> 
      case l of
      IntConstant intL ->
        case r of 
          IntConstant intR -> operation l r
          Variable varName -> expression
          BinaryTerm operation lhv rhv -> operation l (evaluate r)
      Variable varName -> expression
      BinaryTerm operationL lhvL rhvL ->
        case r of
          IntConstant intValue -> operation (evaluate l) r
          Variable varName -> expression
          BinaryTerm operationR lhvR rhvR -> operation (evaluate l) (evaluate r)

infixl 6 |+|, |-|
infixl 7 |*|