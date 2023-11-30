{-# LANGUAGE NamedFieldPuns #-}
module Features where

import Prelude hiding ((<>))
import DateTime
import Calendar
import Text.PrettyPrint.Boxes
import Data.Time.Clock
import Data.Time (fromGregorian, gregorianMonthLength)
import qualified Data.Time.Calendar as Cal


-- Exercise 9
countEvents :: Calendar -> Int
countEvents Calendar{events} = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt Calendar{events} = filter checkDate events
    where
        checkDate :: Event -> Bool
        checkDate Event{dtstart, dtend} = let dtNormalized = normalizeUTC dt
                                           in dtNormalized >= normalizeUTC dtstart && dtNormalized < normalizeUTC dtend
            where
                -- We're ignoring UTC here, but the derived Ord instance for DateTime will use it in the comparison.
                -- Always set it to True when comparing foregoes this
                normalizeUTC :: DateTime -> DateTime
                normalizeUTC d = d{utc = True}

checkOverlapping :: Calendar -> Bool 
checkOverlapping c@Calendar{events} = notNull overlapping
    where
        -- Generate a list with events that overlap
        overlapping :: [Event]
        overlapping = [e | e@Event{dtstart, dtend} <- events, hasDifferentOverlapping e]
            where
                -- Filter out the event itself from the calendar, then check for overlap with other events using its dtstart and dtend
                hasDifferentOverlapping :: Event -> Bool
                hasDifferentOverlapping e@Event{dtstart, dtend} =
                    let filteredCal = c{events = filter (/=e) events}
                     in notNull (findEvents dtstart filteredCal) || notNull (findEvents dtend filteredCal)

        notNull :: [a] -> Bool
        notNull = not . null

timeSpent :: String -> Calendar -> Integer
timeSpent s Calendar{events} = sum $ map durationInMinutes (filter compareSummary events)
    where
        compareSummary :: Event -> Bool
        compareSummary Event{summary} = maybe False (== s) summary

        durationInMinutes :: Event -> Integer
        durationInMinutes Event{dtstart, dtend} = dateDiffMinutes dtstart dtend

        dateDiffMinutes :: DateTime -> DateTime -> Integer
        dateDiffMinutes (DateTime ds ts _) (DateTime de te _) =
            let timeUTCs = UTCTime (dateToDay ds) (timeToDiffTime ts)
                timeUTCe = UTCTime (dateToDay de) (timeToDiffTime te)
                in round (nominalDiffTimeToSeconds (diffUTCTime timeUTCe timeUTCs)) `div` 60
            where
                timeToDiffTime :: Time -> DiffTime
                timeToDiffTime (Time (Hour h) (Minute m) (Second s)) = secondsToDiffTime (toInteger (h * 3600 + m * 60 + s))

                dateToDay :: Date -> Cal.Day
                dateToDay (Date (Year y) (Month m) (Day d)) = fromGregorian (toInteger y) m d

-- Exercise 10
ppMonth :: Year -> Month -> Calendar -> String
ppMonth (Year y) (Month m) c = render (dayBox 1)
    where
        dayBox n = text ('+' : replicate columnWidth '-' ++ "+") // text ('|' : rightPadToN columnWidth (' ' : show n) ++ "|")
        columnWidth = 15
        rightPadToN :: Int -> String -> String
        rightPadToN n s = let d = n - length s
                           in s ++ replicate d ' '
        monthLength = gregorianMonthLength (toInteger y) m
