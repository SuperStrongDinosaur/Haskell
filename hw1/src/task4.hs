{-# LANGUAGE InstanceSigs #-}

module Task4 where

import Data.Semigroup

data Pair a = Pair a a

instance Foldable Pair where
    foldMap :: Monoid m => (a -> m) -> Pair a -> m
    foldMap f (Pair x y) = f x `mappend` f y 

    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f z (Pair x y) = f x $ f y z

data NonEmpty a = a :| [a]
    deriving (Show)

instance Foldable NonEmpty where
    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (x :| xs) = f x `mappend` foldMap f xs

    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f z (x :| xs) = f x (foldr f z xs)

instance Semigroup (NonEmpty a) where
    (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys) 
    
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x list = part : parts where
    part = takeWhile (/= x) list
    parts = splitOn x $ drop (length part + 1) list

joinWith :: (Eq a) => a -> [[a]] -> [a]
joinWith _ [] = []
joinWith _ [x] = x
joinWith a (b : bs) = foldl (\x y -> x ++ [a] ++ y) b bs