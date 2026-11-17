{-# LANGUAGE InstanceSigs #-}

module Part4.Tasks where

import Util (NotImplementedYet, notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (:<) (ReverseList a) a

infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
  reverse (reversed lst)
  where
    reversed REmpty = []
    reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = convert . reverse
  where
    convert :: [a] -> ReverseList a
    convert [] = REmpty
    convert (hd : tl) = convert tl :< hd

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
  show lst = concat ["[", convert lst, "]"]
    where
      convert REmpty = ""
      convert (REmpty :< hd) = show hd
      convert (tl :< hd) = concat [convert tl, ",", show hd]

instance (Eq a) => Eq (ReverseList a) where
  (==) REmpty REmpty = True
  (==) REmpty _ = False
  (==) _ REmpty = False
  (==) ((:<) tl1 hd1) ((:<) tl2 hd2) = (hd1 == hd2) && (tl1 == tl2)

instance Semigroup (ReverseList a) where
  (<>) :: ReverseList a -> ReverseList a -> ReverseList a
  (<>) a REmpty = a
  (<>) a ((:<) tl hd) = (a <> tl) :< hd

instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap :: (a -> b) -> ReverseList a -> ReverseList b
  fmap f REmpty = REmpty
  fmap f ((:<) tl hd) = (REmpty <> fmap f tl) :< f hd

instance Applicative ReverseList where
  pure :: a -> ReverseList a
  pure = (:<) REmpty
  
  (<*>) :: ReverseList (a -> b) -> ReverseList a -> ReverseList b
  -- каждую функцию надо применить к каждому элементу
  (<*>) REmpty _ = REmpty
  (<*>) (ftl :< fhd) a = ftl <*> (fmap fhd a)

instance Monad ReverseList where
  (>>=) a b = REmpty
