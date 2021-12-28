module Task3_civ where 

import Task4 

data Lord = Lord 
    deriving (Show)
data Walls = Walls 
    deriving (Show)

data House = One | Two | Three | Four
    deriving (Show)
data Castle = Castle { lord :: Maybe Lord, walls :: Maybe Walls }
    deriving (Show)
data Building = Church | Library
    deriving (Show)
data City = City { castle :: Maybe Castle, building :: Maybe Building, houses :: NonEmpty House}
    deriving (Show)

buildCastle :: City -> (Bool, Maybe City)
buildCastle (City Nothing a b) = (True, Just $ City (Just (Castle Nothing Nothing)) a b) 
buildCastle (City (Just (Castle _ _)) _ _) = (False, Nothing) -- have castle

buildBuilding :: City -> (Bool, Maybe City)
buildBuilding (City a Nothing b) = (True, Just $ City a (Just Church) b)
buildBuilding (City _ (Just Church) _) = (False, Nothing) --Already have church
buildBuilding (City _ (Just Library) _) = (False, Nothing) --Already have library

buildHome :: City -> House -> City
buildHome (City a b (y :| ys)) x = City a b (x :| (y : ys))

buildLord :: City -> (Bool, Maybe City)
buildLord (City Nothing _ _) = (False, Nothing) --No castle for lord
buildLord (City (Just (Castle (Just Lord) _)) _ _) = (False, Nothing) --Alredy have lord
buildLord (City (Just (Castle Nothing a)) b c) = (True, Just $ City (Just (Castle (Just Lord) a)) b c)

sumOfList :: [House] -> Int
sumOfList [] = 0
sumOfList (One : xs) = 1 + sumOfList xs
sumOfList (Two : xs) = 2 + sumOfList xs
sumOfList (Three : xs) = 3 + sumOfList xs
sumOfList (Four : xs) = 4 + sumOfList xs

sumOfPeople :: NonEmpty House -> Int
sumOfPeople (One :| a) = 1 + sumOfList a
sumOfPeople (Two :| a) = 2 + sumOfList a
sumOfPeople (Three :| a) = 3 + sumOfList a
sumOfPeople (Four :| a) = 4 + sumOfList a    

buildWalls :: City -> (Bool, Maybe City)
buildWalls (City Nothing _ _) = (False, Nothing) --No castle for walls
buildWalls (City (Just (Castle Nothing _)) _ _) = (False, Nothing) --Already have walls
buildWalls (City (Just (Castle (Just Lord) (Just _))) _ _) = (False, Nothing) --No lord for walls
buildWalls (City (Just (Castle (Just Lord) Nothing)) b y) = if sumOfPeople y < 10 
                                                                    then (False, Nothing) -- less than 10 people
                                                                    else (True, Just $ City (Just (Castle (Just Lord) (Just Walls))) b y)


