module Task5 where

import Data.Semigroup

maybeConcat :: (Monoid a) => [Maybe a] -> a
maybeConcat = foldr conc mempty
   where
       conc (Just x) y = x `mappend` y
       conc Nothing y = y

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr conc (mempty, mempty)
   where 
       conc (Left a) (l, r) = (a `mappend` l, r)
       conc (Right a) (l, r) = (l, a `mappend` r)


data ThisOrThat a b = This a | That b | Both a b
    deriving (Show)
 
instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (This a) <> (This b) = This (a <> b)
    (That a) <> (That b) = That (a <> b)
    (That a) <> (This b) = Both b a
    (This a) <> (That b) = Both a b
    (This a) <> (Both c d) = Both (a <> c) d
    (That a) <> (Both c d) = Both c (a <> d)
    (Both a b) <> (That c) = Both a (b <> c)
    (Both a b) <> (This c) = Both (a <> c) b
    (Both a b) <> (Both c d) = Both (a <> c) (b <> d)

newtype Name = Name String
    deriving (Show, Eq)

instance Semigroup Name where
    (<>) = mappend

instance Monoid Name where
    mempty = Name ""
    (Name a) `mappend` (Name b) | a == "" = Name b
                                | b == "" = Name a
                                | otherwise = Name $ a ++ "." ++ b 

data Builder = One Char | Many [Builder] 

fromString :: String -> Builder
fromString [a] = One a
fromString a = Many (map One a)

toString :: Builder -> String
toString (One a) = [a]
toString (Many a) = foldMap toString a

instance Semigroup Builder where
    x <> y = x `mappend` y

instance Monoid Builder where
    mempty = Many []
    Many a `mappend` b = Many (a ++ [b])
    x `mappend` b = Many (x : [b])
    






