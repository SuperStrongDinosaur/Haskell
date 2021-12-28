module Task2 where 

takeAt :: Int -> [a] -> ([a], Maybe a)
takeAt _ [] = ([], Nothing)
takeAt 0 (x : xs) = (xs, Just x)
takeAt i (x : xs) = let a = takeAt (i - 1) xs in (x : fst a, snd a)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = let mid = length x `div` 2 in merge (mergeSort $ take mid x) (mergeSort $ drop mid x) where
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] r = r
    merge l [] = l
    merge (l : ls) (r : rs) | l <= r = l : merge ls (r : rs)
                            | otherwise = r : merge (l : ls) rs