{-# LANGUAGE InstanceSigs #-}

module Task3_tree where 

data Tree a = Leaf | Node a (Tree a) (Tree a)

empty :: Tree a -> Bool
empty Leaf = True
empty Node{} = False

size :: Tree a -> Int
size Leaf = 0
size (Node _ l r) = size l + size r + 1

contains :: Ord a => Tree a -> a -> Bool
contains Leaf _ = False
contains (Node a l r) x | a == x = True
                               | x < a = contains l x
                               | otherwise = contains r x

insert :: Ord a => Tree a -> a -> Tree a       
insert Leaf x = Node x Leaf Leaf
insert (Node a l r) x | a == x = Node x l r
                             | x < a = Node a (insert l x) r
                             | otherwise = Node a l (insert r x)

fromList :: Ord a => [a] -> Tree a
fromList = foldl insert Leaf 

toList :: Ord a => Tree a -> [a]
toList Leaf = []
toList (Node a l r) = toList l ++ [a] ++ toList r

delete :: Ord a => Tree a -> a -> Tree a      
delete Leaf _ = undefined
delete (Node a l r) x | a == x = 
                            case (l, r) of
                                (Leaf, Leaf) -> Leaf
                                (Leaf, n@Node{}) -> n
                                (n@Node{}, Leaf) -> n
                                (nl, nr) -> let mx = leftmost nr in Node mx nl (delete nr mx)
                      | x < a = Node a (delete l x) r 
                      | otherwise = Node a l (delete r x) 

leftmost :: Tree a -> a
leftmost Leaf = undefined
leftmost (Node x Leaf _) = x
leftmost (Node _ l _) = leftmost l

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf = mempty
    foldMap f (Node a l r) = foldMap f l `mappend` f a `mappend` foldMap f r

    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ x Leaf = x
    foldr f x (Node a l r) = foldr f (f a $ foldr f x r) l

                      

