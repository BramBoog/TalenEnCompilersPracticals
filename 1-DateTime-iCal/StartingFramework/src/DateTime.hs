module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.List (find)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = (\d _ t u -> DateTime d t u) <$> parseDate <*> symbol 'T' <*> parseTime <*> parseUTC
    where
        parseUTC :: Parser Char Bool
        parseUTC = (== 'Z') <$> satisfy (== 'Z')

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

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
run p s = fmap fst $ find (\(_, r) -> null r) (parse p s)

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
