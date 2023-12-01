{-# LANGUAGE NamedFieldPuns #-}
module Features where

import Prelude hiding ((<>))
import DateTime
import Calendar
import Text.PrettyPrint.Boxes
import qualified Data.Map as M
import Data.List (sort)
import qualified Data.Set as S
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

type TimeRange = (Time, Time)
type DateBuckets = M.Map Date [TimeRange]
data Appointment = Appointment {dateKey :: Date, timeRange :: TimeRange} deriving (Eq, Show)

-- Generate the necessary Appointments to represent the Event in a printed calendar
getAppointments :: Year -> Month -> Event -> [Appointment]
getAppointments y m Event{dtstart, dtend} | dateS == dateE = singleDayAppointment
                                          | otherwise = startDayAppointment ++ endDayAppointment
    where
        DateTime{date = dateS@Date{year = yearS, month = monthS, day = dayS}, time = timeS@Time{hour = hourS, minute = minuteS}} = dtstart
        DateTime{date = dateE@Date{year = yearE, month = monthE, day = dayE}, time = timeE@Time{hour = hourE, minute = minuteE}} = dtend
        
        -- Functions to generate Appointments for the event; if the start and end date are the same, the result is a single Appointment.
        -- If they differ, generate one Appointment until midnight on the start date and another one from midnight on the end date.
        -- NOTE: this assumes appointments never span more than one day, month or year.
        -- Appointments are not generated for dates of which the year and month don't match with the requested year and month
        singleDayAppointment :: [Appointment] 
        singleDayAppointment | y /= yearS || m /= monthS = []
                             | otherwise = [Appointment dateS (timeS, timeE)]

        startDayAppointment :: [Appointment] 
        startDayAppointment | y /= yearS || m /= monthS = []
                            | otherwise = [Appointment dateS (timeS, Time (Hour 23) (Minute 59) (Second 59))]

        endDayAppointment :: [Appointment]
        endDayAppointment | y /= yearE || m /= monthE = []
                          | otherwise = [Appointment dateE (Time (Hour 0) (Minute 0) (Second 0), timeE)]

createDateBuckets :: [Appointment] -> DateBuckets
createDateBuckets = foldr insertAppointment M.empty
    where
        insertAppointment Appointment{dateKey, timeRange} = M.insertWith (++) dateKey [timeRange]

ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m Calendar{events} = render monthBox
    where
        dateBuckets = M.map sort (createDateBuckets (concatMap (getAppointments y m) events))

        monthBox :: Box
        monthBox = vcat top (allWeeks 1)
            where
                allWeeks :: Int -> [Box]
                allWeeks startDay | startDay < numOfDaysInMonth = weekBox startDay wLength : allWeeks (startDay + 7)
                                  | otherwise = []
                    where
                        numOfDaysInMonth = gregorianMonthLength (toInteger $ runYear y) (runMonth m)
                        
                        -- Determine how many days of a week fall in the given month
                        wLength :: Int
                        wLength | monthWeekDayDiff >= 7 = 6
                                | otherwise = monthWeekDayDiff
                            where
                                monthWeekDayDiff = numOfDaysInMonth - startDay

        -- Make a box for a week of appointments, given the startday and the amount of days in the week (which is less than 7 if the month ends before the week)
        weekBox :: Int -> Int -> Box
        weekBox startDay weekLength = hcat left (map createDayBox allDaysOfWeek)
            where
                allDaysOfWeek :: [Date]
                allDaysOfWeek = [Date y m (Day x) | x <- [startDay..(startDay + weekLength)]]
                dateBucketsOfWeek = M.restrictKeys dateBuckets (S.fromList allDaysOfWeek)

                -- Determines how high each of the boxes in the week needs to be      
                maxAppointments :: Int     
                maxAppointments = if M.null dateBucketsOfWeek then 0
                                  else maximum (map (length . snd) (M.toList dateBucketsOfWeek))

                createDayBox date = let trs = M.findWithDefault [] date dateBucketsOfWeek
                                     in dayBox date trs maxAppointments

        -- Provided with a date, list of time ranges for appointments on that date, and height of the box, creates a box representing a day
        dayBox :: Date -> [TimeRange] -> Int -> Box
        dayBox (Date _ _ (Day d)) timeRanges height = lineBox // dayNumberBox d // intermediateBoxes timeRanges height // lineBox
            where
                cellWidth = 15

                fillCellLineWith :: Char -> String
                fillCellLineWith = replicate cellWidth

                -- Creates a box used to separate the other boxes
                lineBox :: Box
                lineBox = text ('+' : fillCellLineWith '-' ++ "+")

                -- Creates a box used to represent the number of day
                dayNumberBox :: Int -> Box
                dayNumberBox n = text ('|' : rightPadInt (' ' : show n) ++ "|")
                    where
                        rightPadInt :: String -> String
                        rightPadInt s = s ++ replicate (cellWidth - length s) ' '

                -- Creates the intermediate box for a day by filling in events (expressed as time ranges) and pads the 
                -- rest of the intermediate box with empty boxes so that it aligns with the rest of the day boxes
                intermediateBoxes :: [TimeRange] -> Int -> Box
                intermediateBoxes trs n = vcat left (map timeRangeBox trs) // vcat left (replicate (n - length trs) emptyBox)
                    where
                        -- Creates an empty box to pad a day box
                        emptyBox :: Box
                        emptyBox = text ('|' : fillCellLineWith ' ' ++ "|") 

                        -- Creates a box with a time range for a day box
                        timeRangeBox :: TimeRange -> Box
                        timeRangeBox tr = text ('|' : showTimeRange tr ++ "|")
                            where
                                showTimeRange :: TimeRange -> String
                                showTimeRange (Time{hour = h1, minute = m1}, Time{hour = h2, minute = m2}) = " " ++ show h1 ++ ":" ++ show m1 ++ " - " ++ show h2 ++ ":" ++ show m2 ++ " "
