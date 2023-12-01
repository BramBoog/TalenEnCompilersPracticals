module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.List (find)
import Data.Maybe (isJust)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = (\d _ t u -> DateTime d t u) <$> parseDate <*> symbol 'T' <*> parseTime <*> parseUTC
    where
        parseUTC :: Parser Char Bool
        parseUTC = isJust <$> optional (symbol 'Z')

        parseDate :: Parser Char Date
        parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

        parseTime :: Parser Char Time
        parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

-- Convert a list of digits into a single number; for example [1, 3, 6] becomes 136
digitsToNum :: [Int] -> Int
digitsToNum xs = fst $ foldr f (0, 0) xs
  where
    f x (acc, dec) = (acc + x * 10 ^ dec, dec + 1)

parse2DigitsInto :: (Int -> a) -> Parser Char a
parse2DigitsInto f = (\d1 d2 -> f (digitsToNum [d1, d2])) <$> newdigit <*> newdigit

parse4DigitsInto :: (Int -> a) -> Parser Char a
parse4DigitsInto f = (\d1 d2 d3 d4 -> f (digitsToNum [d1, d2, d3, d4])) <$> newdigit <*> newdigit <*> newdigit <*> newdigit

parseYear = parse4DigitsInto Year
parseMonth = parse2DigitsInto Month
parseDay = parse2DigitsInto Day
parseHour = parse2DigitsInto Hour
parseMinute = parse2DigitsInto Minute
parseSecond = parse2DigitsInto Second

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = fst <$> find (\(_, r) -> null r) (parse p s)

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date y mo d) (Time h mi s) u) = show y ++ show mo ++ show d ++ 'T' : show h ++ show mi ++ show s ++ showUTC u
    where showUTC b | b = "Z"
                    | otherwise = ""

-- Pad the string representation of an Int with leading 0's up to a certain length
leftPadToN :: Int -> Int -> String
leftPadToN n x = let s = show x
                     d = n - length s
                 in replicate d '0' ++ s

instance Show Year where
    show (Year y) = leftPadToN 4 y

instance Show Month where
    show (Month m) = leftPadToN 2 m

instance Show Day where
    show (Day d) = leftPadToN 2 d

instance Show Hour where 
    show (Hour h) = leftPadToN 2 h

instance Show Minute where 
    show (Minute m) = leftPadToN 2 m

instance Show Second where 
    show (Second s) = leftPadToN 2 s

instance Show DateTime where
    show = printDateTime


-- Exercise 4
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime d t _) = isValidDate d && isValidTime t 

--- Check if an int is in a given range (inclusive on both ends)
isInRange :: Int -> Int -> Int -> Bool
isInRange n l u = n >= l && n <= u

isValidDate :: Date -> Bool
isValidDate (Date (Year y) (Month m) (Day d)) = validYear && validMonth && validDay
    where
    
        isLeapYear = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)
        validYear = (length . show) y <= 4 -- Check that the year value consists of four digits or less
        validMonth = isInRange m 1 12
        validDay | m == 2 = validFebDay -- Handle february case, considering leap years
                 | m < 8 = checkDayRangeOnMonthNumber odd -- If month before August, then odd months should have 31 days and even 30 days
                 | otherwise = checkDayRangeOnMonthNumber even -- If month after July, then odd months should have 30 days and even 31 days
            where
                validFebDay | isLeapYear = isInRange d 1 29 
                            | otherwise =  isInRange d 1 28
                -- Check the day number in a range ending on 30 or 31 based on some predicate that considers the month number
                checkDayRangeOnMonthNumber :: (Int -> Bool) -> Bool
                checkDayRangeOnMonthNumber p | p m = isInRange d 1 31
                                             | otherwise = isInRange d 1 30
                                    
isValidTime :: Time -> Bool
isValidTime (Time (Hour h) (Minute m) (Second s)) = isValidHour && isValidMinSec m && isValidMinSec s
    where
        isValidHour = isInRange h 0 23
        isValidMinSec x = isInRange x 0 59
