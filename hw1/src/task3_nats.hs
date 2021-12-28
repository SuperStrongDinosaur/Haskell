module Task3_nats where 

data Nat = Z | S Nat

fromNat :: Nat -> Integer
fromNat Z = 0
fromNat (S n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Z
toNat n = S . toNat $ n - 1

instance Eq Nat where 
    Z == Z = True
    Z == _ = False 
    _ == Z = False
    (S a) == (S b) = a == b

instance Ord Nat where
    Z <= Z = True
    Z <= _ = True
    _ <= Z = False
    (S a) <= (S b) = a <= b 

instance Num Nat where
    x + Z = x
    x + (S ys) = S (x + ys)

    _ * Z = Z
    x * (S c) = x + x * c

    x - Z = x
    x - (S c) = dec x - c where dec Z = Z
                                dec (S b) = b
    abs x = x
    signum _ = 1

    fromInteger x | x <= 0 = Z
                  | otherwise = S . fromInteger $ x - 1

isEven :: Nat -> Bool
isEven Z = True
isEven (S x) = not $ isEven x 

mmod :: Nat -> Nat -> Nat
mmod _ Z = error "%0"
mmod x y | x >= y = mmod (x - y) y
         | otherwise = x







