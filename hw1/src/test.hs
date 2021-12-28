module Test where 

import Data.Semigroup
import Data.Maybe

import Task1
import Task2
import Task3_days
import Task3_civ
import Task3_tree
import Task3_nats
import Task4
import Task5

--import System.Random (newStdGen, randomRs)
--randomIntList :: Int -> Int -> Int -> IO [Int]
--randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

test_task1 = do
   print "task1"

   print "order3:"
   print (3, 1, 2)
   print (order3 (3, 1, 2)) 

   print "smartReplicate:"
   print [1,2,3]
   print (smartReplicate [1,2,3])

   print "contains:"
   print 3
   print [[1..5], [2,0], [3,4]]
   print [[1,2,3,4,5],[3,4]]

   print "stringSum:"
   print "1 1"
   print (stringSum "1 1")
   print "100\n\t-3"
   print (stringSum "100\n\t-3")

test_task2 = do
   print "task2"

   print "takeAt:"
   print 3
   print [2, 1, 0, 3, 10, 5]
   print (takeAt 3 [2, 1, 0, 3, 10, 5])
   print 10
   print [2, 1, 0, 3, 10, 5]
   print (takeAt 10 [2, 1, 0, 3, 10, 5])

   print "mergeSort:"
   print [2, 1, 0, 3, 10, 5]
   print (mergeSort [2, 1, 0, 3, 10, 5])

test_task3 = do
   print "task3"
   print "Days:"

   print "nextDay:"
   print Sun
   print (nextDay Sun)

   print "afterDays:"
   print 5
   print Thu
   print (afterDays 5 Thu)

   print "isWeekend:"
   print Sun
   print (isWeekend Sun)
   print Thu
   print (isWeekend Thu)

   print "daysToParty:"
   print Wed
   print (daysToParty Wed)

   print ""
   print "Castle"

   let a = City Nothing Nothing (Two :| [Two]) 
   print a
   print "buildCastle"
   print (buildCastle a)
   print "buildBuilding"
   print (buildBuilding a)
   print "buildHome Four"
   print (buildHome a Four)
   let b = City {castle = Just (Castle {lord = Nothing, walls = Nothing}), building = Nothing, houses = Four :| [Four, Four]}
   print b
   print "buildLord"
   print (buildLord b)
   print "buildWalls"
   print (buildWalls $ fromJust $ snd $ buildLord b)

   print ""
   print "Nats"
   print "45+23"
   print (fromNat $ (toNat 45) + (toNat 23))
   print "45-23"
   print (fromNat $ (toNat 45) - (toNat 23))
   print "45*23"
   print (fromNat $ (toNat 45) * (toNat 23))
   print "45<23"
   print ((toNat 45) < (toNat 23))
   print "45>=23"
   print ((toNat 45) >= (toNat 23))
   print "isEven 25"
   print (isEven $ toNat 25)
   print "45%23"
   print (fromNat $ mmod (toNat 45) (toNat 23))

   print ""
   print "Tree"

   print "empty:"
   print [1, 2, 5, 4]
   print (empty (fromList [1, 2, 5, 4]))

   print "contains"
   print [1, 2, 5, 4]
   print 4
   print (Task3_tree.contains (fromList [1, 2, 5, 4]) 4)
   print [1, 2, 5, 4]
   print 6
   print (Task3_tree.contains (fromList [1, 2, 5, 4]) 6)

   print "insert"
   print [1, 3, 5, 4]
   print 2
   print (toList (Task3_tree.insert (fromList [1, 3, 5, 4]) 2))
   print [1, 2, 5, 4]
   print 1
   print (toList (Task3_tree.insert (fromList [1, 2, 5, 4]) 1))

   print "delete"
   print [1, 3, 5, 4]
   print 3
   print (toList (Task3_tree.delete (fromList [1, 3, 5, 4]) 3))
   print [1, 2, 5, 4]
   print 2
   print (toList (Task3_tree.delete (fromList [1, 2, 5, 4]) 2))

test_task4 = do
   print "task4"

   print "splitOn"
   print ["/", "path/to/file"]
   print (splitOn '/' "path/to/file")

   print "joinWith"
   print ["/", "path, to, file"]
   print (joinWith '/' ["path", "to", "file"])

   print "Foldable for Pair"
   print "+ 20 10 5"
   print (foldr (+) 20 (Pair 10 5))

   print "Foldable for NonEmpty"
   print "+ 20 10 :| 5"
   print (foldr (+) 20 (10 :| [5]))

test_task5 = do
   print "task5"

   print "maybeConcat"
   print [Just [1,2,3], Nothing, Just [4,5]]
   print (maybeConcat [Just [1,2,3], Nothing, Just [4,5]])

   print "EitherCoccat"
   print [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
   print (eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]])

   print "Semigroup for NonEmpty"
   print [[1, 2], [2, 3]]
   print ((1 :| [2]) <> (2 :| [3]))

   print "Monoid for Name"
   print "lol rofl"
   print ((Name "lol") `mappend` (Name "rofl"))

   print "Semigroup for Builder"
   print ["lol", "rofl"]
   print (toString((fromString "lol") <> (fromString "rofl")))























