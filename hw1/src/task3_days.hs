module Task3_days where 

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

dayToNum :: DayOfWeek -> Int
dayToNum Mon = 0
dayToNum Tue = 1
dayToNum Wed = 2
dayToNum Thu = 3
dayToNum Fri = 4
dayToNum Sat = 5
dayToNum Sun = 6

numToDay :: Int -> DayOfWeek
numToDay 0 = Mon
numToDay 1 = Tue
numToDay 2 = Wed
numToDay 3 = Thu
numToDay 4 = Fri
numToDay 5 = Sat
numToDay 6 = Sun
numToDay n = numToDay $ n `mod` 7

afterDays :: Int -> DayOfWeek -> DayOfWeek
afterDays n day = numToDay $ dayToNum day + n

nextDay :: DayOfWeek -> DayOfWeek
nextDay = afterDays 1

isWeekend :: DayOfWeek -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

daysToParty :: DayOfWeek -> Int
daysToParty day = (dayToNum Fri - dayToNum day) `mod` 7