module Part2.Tasks where

import GHC.Num.Integer (Integer (IN))
import Util (notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term
  = IntConstant {intValue :: Int} -- числовая константа
  | Variable {varName :: String} -- переменная
  | BinaryTerm {op :: BinaryOp, lhv :: Term, rhv :: Term} -- бинарная операция
  deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

infixl 6 |-|

infixl 6 |+|

infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar _ _ term@(IntConstant _) = term
replaceVar name rname term@(Variable varName)
  | name == varName = rname
  | otherwise = term
replaceVar name rname term@(BinaryTerm op lhv rhv) = BinaryTerm op (replaceVar name rname lhv) (replaceVar name rname rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus (IntConstant l) (IntConstant r)) = IntConstant (l + r)
evaluate (BinaryTerm Minus (IntConstant l) (IntConstant r)) = IntConstant (l - r)
evaluate (BinaryTerm Times (IntConstant l) (IntConstant r)) = IntConstant (l * r)
evaluate term@(BinaryTerm op lhv rhv) = case (evaluate lhv, evaluate rhv) of
  (l@(IntConstant _), r@(IntConstant _)) -> evaluate (BinaryTerm op l r)
  _ -> term
evaluate term = term
