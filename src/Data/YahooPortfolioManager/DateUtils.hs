{- |
Date/time utilities
-}

module Data.YahooPortfolioManager.DateUtils where

import Data.Time
import Data.Time.Calendar.WeekDate

data TimePeriod = BusDay | Month | Year

{-| Get todays date from the system clock -}
getDate :: IO Day
getDate = do
        date <- getCurrentTime
        return $ utctDay date

{-| Convert a string in the format yyyy-mm-dd to a Day -}
toDate :: String -> Day
toDate d = read d :: Day

{-| Get the fraction of a year between the two input dates, t2 > t1 -}
coverage :: Day -> Day -> Double
coverage t1 t2 = (fromIntegral elapsedDays) / (fromIntegral daysInPastYear)
        where
        elapsedDays = diffDays t2 t1
        daysInPastYear = diffDays t2 $ addGregorianYearsRollOver (-1) t2

{-| Compare to dates using the supplied comparison function -}
compare :: (Integer -> Integer -> Bool) -> Day -> Day -> Bool
compare f t1 t2 = f (toModifiedJulianDay t1) (toModifiedJulianDay t2)

{-| Get the weekdays for the specified time period, as measured from
the requested date
-}
getWeekDays :: Integer -> TimePeriod -> Day -> [Day]
getWeekDays offset period d = filter isWeekDay [min d d0 .. max d d0]
    where d0 = case period of
                BusDay  -> addDays offset d 
                Month   -> addGregorianMonthsClip offset d
                Year    -> addGregorianYearsClip  offset d

isWeekDay :: Day -> Bool
isWeekDay day = d /= 6 && d /= 7
    where (_, _, d) = toWeekDate day
