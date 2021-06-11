module HashMap where

import Prelude hiding (lookup)

-- | HashMap backed by a binary tree. Most operations are O(log(n))
data HashMap k v = EmptyMap | Map (k, v) (HashMap k v) (HashMap k v) deriving (Read, Show)


-- | Apply fmap over all values
instance (Ord k) => Functor (HashMap k) where
  fmap _ EmptyMap = EmptyMap
  fmap f (Map (key, value) leftMap rightMap) =
    Map (key, f value) (fmap f leftMap) (fmap f rightMap)


-- | Inserts a 2-tuple key-value pair into a HashMap
insert :: (Ord k) => (k, v) -> HashMap k v -> HashMap k v
insert pair EmptyMap = Map pair EmptyMap EmptyMap
insert (key, value) (Map (mapKey, mapValue) leftMap rightMap) =
  case key `compare` mapKey of
    GT        -> Map (mapKey, mapValue) leftMap (insert (key, value) rightMap)
    otherwise -> Map (mapKey, mapValue) (insert (key, value) leftMap) rightMap

-- | Lookup corresponding value for key
lookup :: (Ord k) => k -> HashMap k v -> Maybe v
lookup _ EmptyMap = Nothing
lookup key (Map (mapKey, mapValue) leftMap rightMap) =
  case key `compare` mapKey of
    EQ -> Just mapValue
    LT -> lookup key leftMap
    GT -> lookup key rightMap


-- | Applies function to value at corresponding key
update :: (Ord k) => (v -> v) -> k -> HashMap k v -> HashMap k v
update _ _ EmptyMap = EmptyMap
update f key (Map (mapKey, mapValue) leftMap rightMap) =
  case key `compare` mapKey of
    EQ -> Map (mapKey, f mapValue) leftMap rightMap
    LT -> Map (mapKey, mapValue) (update f key leftMap) rightMap
    GT -> Map (mapKey, mapValue) leftMap (update f key rightMap)

-- | Create a map from a list of 2-tuple key-value pairs
fromList :: (Ord k) => [(k, v)] -> HashMap k v
fromList = foldr insert EmptyMap


-- | Example usage
main :: IO ()
main = do
  let capitals = fromList [("England", "London"),
                           ("Myanmar", "Nay Pyi Taw"),
                           ("France", "Paris"),
                           ("Italy", "Roma"),
                           ("Russia", "Moscow"),
                           ("Spain", "Madrid")]
                 
      capitalsWithJapan = insert ("Japan", "Tokyo") capitals

  -- Retreive Just "London" from updated capitals map
  print $ lookup "England" capitalsWithJapan

  -- Receive Just dirdaM from fmapped updated capitals
  print $ lookup "Spain" (fmap reverse capitalsWithJapan)
  
  -- Receive Nothing from updated capitals map
  print $ lookup "Russia" capitalsWithJapan
