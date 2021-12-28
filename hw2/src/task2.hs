{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Task2 where

import Text.Read

stringSum :: String -> Maybe Int
stringSum s = sum <$> traverse f (words s) where
  f :: String -> Maybe Int
  f ('+' : xs) = readMaybe xs
  f xs = readMaybe xs

newtype Optional a = Optional (Maybe (Maybe a))
  deriving (Show)

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap f (Optional (Just (Just a))) = Optional (Just (Just (f a))) -- fmap (+40) (Optional (Just (Just 2))) === Optional (Just (Just (40 + 2)) === Optional (Just (Just (42))
  fmap _ (Optional _) = Optional Nothing -- fmap (+40) (Optional Nothing) === Optional Nothing

instance Applicative Optional where
  pure :: a -> Optional a
  pure a = Optional (Just (Just a)) 

  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  (Optional (Just (Just a))) <*> some = fmap a some -- Optional (Just (Just (+2))) <*> Optional (Just (Just 2)) ===
  (Optional _) <*> _ = Optional Nothing             -- fmap (+2) Optional (Just (Just 2)) ===
                                                    -- Optional (Just (Just (2 + 2))) === Optional (Just (Just (4)))
instance Monad Optional where
  return :: a -> Optional a
  return a = Optional (Just (Just a)) 

  (>>=) :: Optional a -> (a -> Optional b) -> Optional b
  (Optional (Just (Just a))) >>= f = f a -- Optional (Just (Just 2)) >>= (\x -> Optional $ Just $ Just $ x + 3) ===  Optional (Just (Just (2 + 3))) === Optional (Just (Just 5)
  (Optional _) >>= _ = Optional Nothing

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse f (Optional (Just (Just x))) = Optional . Just . Just <$> f x -- traverse (\x -> Task2.Optional (Just (Just (x + 2)))) (Task2.Optional (Just (Just 2))) === Optional (Just (Just 4)
  traverse _ (Optional _)  = pure (Optional Nothing)
  
instance Foldable Optional where
  foldr :: (a -> b -> b) -> b -> Optional a -> b
  foldr f z (Optional (Just (Just x))) = f x z  --foldr (+) 1 (Optional (Just (Just 4))) === 1 + 4 === 5 
  foldr _ z (Optional _) = z

data NonEmpty a = a :| [a]

toList :: NonEmpty a -> [a]
toList (a :| as) = a : as

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (a :| as) = f a :| fmap f as

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure a = a :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (a :| as) <*> (b :| bs) = a b :| [f x | f <- as, x <- bs]  

instance Monad NonEmpty where
  return :: a -> NonEmpty a
  return a = a :| []

  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (a :| as) >>= f = b :| (bs ++ bs') where 
    b :| bs = f a
    bs' = as >>= toList . f

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (a :| as) = (:|) <$> f a <*> traverse f as

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (a :| as) = f a `mappend` foldMap f as





  