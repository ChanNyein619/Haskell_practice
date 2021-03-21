module Lab2.TreeMap
  ( height,
    key,
    value,
    left,
    right,
    singleton,
    insert,
    delete,
    member,
    notMember,
    get,
    fromList,
    toList,
    TreeMap (Nil, Node),
  )
where

data TreeMap k v = Nil | Node Int k v (TreeMap k v) (TreeMap k v)

height :: TreeMap k v -> Int
height Nil = 0
height (Node h _ _ _ _) = h

key :: TreeMap k v -> k
key Nil = error "key Nil"
key (Node _ k _ _ _) = k

value :: TreeMap k v -> v
value Nil = error "value Nil"
value (Node _ _ v _ _) = v

left :: TreeMap k v -> TreeMap k v
left Nil = error "left Nil" 
left (Node _ _ _ l _) = l

right :: TreeMap k v -> TreeMap k v
right Nil = error "right Nil" 
right (Node _ _ _ _ r) = r

singleton :: k -> v -> TreeMap k v
singleton k v = node k v Nil Nil

insert :: Ord k => k -> v -> TreeMap k v -> TreeMap k v
insert k v Nil = singleton k v
insert k1 v1 (Node _ k2 v2 l r) =
  balance $
    if k1 < k2
      then node k2 v2 (insert k1 v1 l) r
      else
        if k1 /= k2
          then node k2 v2 l (insert k1 v1 r)
          else error "duplicate key"

delete :: Ord k => k -> TreeMap k v -> TreeMap k v
delete _ Nil = Nil
delete k1 (Node _ k2 v2 l2 r2) =
  balance $
    if k1 < k2
      then node k2 v2 (delete k1 l2) r2
      else
        if k1 > k2
          then node k2 v2 l2 (delete k1 r2)
          else case l2 of
            Nil -> r2
            _ -> node minK minV (removeMin l2) r2
  where
    minN = findMin l2
    minK = key minN
    minV = value minN

member :: Ord k => k -> TreeMap k a -> Bool
member k t = case get k t of
  Nothing -> False
  Just _ -> True

notMember :: Ord k => k -> TreeMap k a -> Bool
notMember k m = not $ member k m

get :: Ord k => k -> TreeMap k a -> Maybe a
get _ Nil = Nothing
get k (Node _ k2 v2 l r) =
  case compare k k2 of
    LT -> get k l
    GT -> get k r
    EQ -> Just v2

node :: k -> v -> TreeMap k v -> TreeMap k v -> TreeMap k v
node k v l r = Node h k v l r where h = max (height l) (height r) + 1

bFactor :: TreeMap k v -> Int
bFactor Nil = 0
bFactor (Node _ _ _ l r) = height r - height l

rotateR, rotateL :: TreeMap k v -> TreeMap k v
rotateR (Node _ k1 v1 (Node _ k2 v2 l2 r2) r1) = node k2 v2 l2 (node k1 v1 r2 r1)
rotateR (Node _ _ _ Nil _) = error "rotateR Nil"
rotateR Nil = undefined
rotateL (Node _ k1 v1 l1 (Node _ k2 v2 l2 r2)) = node k2 v2 (node k1 v1 l1 l2) r2
rotateL (Node _ _ _ _ Nil) = error "rotateL Nil"
rotateL Nil = undefined

balance :: TreeMap k v -> TreeMap k v
balance Nil = Nil
balance n@(Node h k v l r)
  | h <= 2 = n
  | bFactorN == 2 = rotateL $ node k v l $ if bFactor r < 0 then rotateR r else r
  | bFactorN == -2 = rotateR $ node k v (if bFactor l > 0 then rotateL l else l) r
  | otherwise = n
  where
    bFactorN = bFactor n

removeMin :: TreeMap k v -> TreeMap k v
removeMin Nil = Nil
removeMin (Node _ k v l r) = balance $
  case l of
    Nil -> case r of
      Nil -> Nil
      _ -> node (key r) (value r) Nil Nil
    _ -> node k v (removeMin l) r

findMin :: TreeMap k v -> TreeMap k v
findMin Nil = undefined
findMin n@(Node _ _ _ l _) =
  case l of
    Nil -> n
    _ -> findMin l

fromList :: Ord k => [(k, v)] -> TreeMap k v
fromList xs = go xs Nil
  where
    go ((k, v) : ys) t = go ys $ insert k v t
    go [] t = t

toList :: TreeMap k v -> [(k,v)]
toList Nil = []
toList (Node _ k v l r) = toList l ++ [(k,v)] ++ toList r

-- There's better method described in this article
-- http://espressocode.top/merge-two-balanced-binary-search-trees/
-- but I wrote a simpler one.
instance (Ord k) => Semigroup (TreeMap k v) where
  x <> Nil = x
  Nil <> x = x
  (Node _ k v l r) <> t2 = l <> r <> insert k v t2

instance Foldable (TreeMap k) where
  foldr _ ini Nil = ini
  foldr f ini (Node _ _ v l r) = foldr f (v `f` foldr f ini r) l

  length = foldl (\s _ -> s + 1) 0
  
instance Functor (TreeMap k) where
  fmap _ Nil = Nil
  fmap f (Node h k v l r) = Node h k (f v) (fmap f l) (fmap f r)

-- I believe these trees are equal.
--     3,3       2,2
--     / \       / \
--   2,2 4,4   1,1 3,3
--   /               \
-- 1,1               4,4
-- They are equals because we have the same key-value pairs.
instance (Eq k, Eq v) => Eq (TreeMap k v) where
  t1 == t2 = toList t1 == toList t2

-- Shows in json format
instance (Show k, Show v) => Show (TreeMap k v) where
  showsPrec _ x = go x
    where
      go Nil = ("null" ++)
      go (Node h k v l r) =
        ("{\"height\": " ++)
          . shows h
          . (", \"key\": " ++)
          . ('"' :)
          . shows k
          . ('"' :)
          . (", \"value\": " ++)
          . ('"' :)
          . shows v
          . ('"' :)
          . (", \"left\": " ++)
          . go l
          . (", \"right\": " ++)
          . go r
          . ('}' :)