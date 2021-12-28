module Task1 where 

import Data.List

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z) where [x, y, z] = sort [a, b, c]

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter $ elem x

stringSum :: String -> Int
stringSum x = sum $ map read $ words x
