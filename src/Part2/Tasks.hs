module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm Plus a b
infixl 5 |+|
(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm Minus a b
infixl 5 |-|
(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm Times a b
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar targetName replacement (Variable varName) = if targetName == varName then replacement else Variable varName
replaceVar targetName replacement (BinaryTerm op lhv rhv) = BinaryTerm op (replaceVar targetName replacement lhv) (replaceVar targetName replacement rhv)
replaceVar varName replacement expression = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate = notImplementedYet
