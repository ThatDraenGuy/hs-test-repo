module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = 
    reversed (reverse lst)
    where reversed [] = REmpty
          reversed (h:t) = (reversed t) :< h

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    showsPrec p lst = showChar '[' . show' lst . showChar ']'
        where show' REmpty = showString ""
              show' (REmpty :< t) = showsPrec p t
              show' (h :< t) = show' h . showChar ',' . showsPrec p t

instance (Eq a) => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) REmpty _ = False
    (==) _ REmpty = False
    (==) (h1 :< t1) (h2 :< t2) = if t1 == t2 then h1 == h2 else False
    (/=) a b = not (a == b)

instance Semigroup (ReverseList a) where
    (<>) a (REmpty) = a
    (<>) a (REmpty :< t) = a :< t
    (<>) a (h :< t) = (a <> h) :< t

instance Monoid (ReverseList a) where
    mempty = REmpty
    mappend a b = a <> b

instance Functor ReverseList where
    fmap f REmpty = REmpty
    fmap f (h :< t) = (fmap f h) :< (f t)

instance Applicative ReverseList where
    pure a = REmpty :< a
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty
    (<*>) (h1 :< t1) (h2 :< t2) = (h1 <*> h2) <> (fmap t1 h2) <> (fmap (\x -> x t2) h1) :< (t1 t2)

instance Monad ReverseList where
    (>>=) lst f = flatten (fmap f lst)
        where flatten REmpty = REmpty 
              flatten (h :< t) = flatten h <> t