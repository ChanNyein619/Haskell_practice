module Lab2.List where

import Data.Foldable (toList)
import Data.Semigroup ()

data List a = Nil | Item a (List a)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Semigroup (List a) where
  list <> Nil = list
  Nil <> list = list
  (Item value list1) <> list2 = Item value $ list1 <> list2

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Item v list) = Item (f v) $ fmap f list

instance Foldable List where
  foldr _ ini Nil = ini
  foldr f ini (Item item list) = item `f` foldr f ini list

  length = foldl (\s _ -> s + 1) 0

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  _ == Nil = False
  Nil == _ = False
  Item a1 l1 == Item a2 l2 = a1 == a2 && l1 == l2

instance (Show a) => Show (List a) where
  showsPrec _ x = shows $ toList x

add :: a -> List a -> List a
add = Item

delete :: Eq a => a -> List a -> List a
delete _ Nil = Nil
delete a (Item x list) = if a == x then list else Item x $ delete a list

head :: List a -> a
head Nil = error "empty list"
head (Item item _) = item

tail :: List a -> List a
tail Nil = error "empty list"
tail (Item _ list) = list

fromList :: [a] -> List a
fromList = foldr Item Nil