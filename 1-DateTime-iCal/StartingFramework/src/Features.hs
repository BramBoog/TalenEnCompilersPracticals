{-# LANGUAGE NamedFieldPuns #-}
module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes
import Data.Time.Clock
import Data.Time (fromGregorian)
import qualified Data.Time.Calendar as Cal


-- Exercise 9
countEvents :: Calendar -> Int
countEvents Calendar{events} = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt Calendar{events} = filter checkDate events
    where
        checkDate :: Event -> Bool 
        checkDate Event{dtstart, dtend} = dt >= dtstart && dt < dtend

checkOverlapping :: Calendar -> Bool
checkOverlapping c@Calendar{events} = notNull overlapping
    where
        overlapping :: [Event]
        overlapping = [e | e@Event{dtstart, dtend} <- events, notNull (findEvents dtstart c) || notNull (findEvents dtend c)]
        notNull :: [a] -> Bool
        notNull = not . null

dateDiffMinutes :: DateTime -> DateTime -> Integer
dateDiffMinutes (DateTime d1 t1 _) (DateTime d2 t2 _) =
    let timeUTC1 = UTCTime (dateToDay d1) (timeToDiffTime t1)
        timeUTC2 = UTCTime (dateToDay d2) (timeToDiffTime t2)
        in round (nominalDiffTimeToSeconds (diffUTCTime timeUTC1 timeUTC2)) `div` 60
    where
        timeToDiffTime :: Time -> DiffTime
        timeToDiffTime (Time (Hour h) (Minute m) (Second s)) = secondsToDiffTime (toInteger h * 3600 + toInteger m * 60 + toInteger s)

        dateToDay :: Date -> Cal.Day
        dateToDay (Date (Year y) (Month m) (Day d)) = fromGregorian (toInteger y) m d

timeSpent :: String -> Calendar -> Integer
timeSpent s Calendar{events} = sum $ map calculateMin (filter compareSummary events)
    where
        compareSummary Event{summary} = maybe False (== s) summary
        calculateMin Event{dtstart, dtend} = dateDiffMinutes dtstart dtend

-- Exercise 10
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined
