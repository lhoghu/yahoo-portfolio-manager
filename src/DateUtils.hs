{- |
Date/time utilities
-}

module DateUtils where

import Data.Time

{- Get todays date from the system clock -}
getDate :: IO Day
getDate = do
        date <- getCurrentTime
        return $ utctDay date

{- Convert a string in the format yyyy-mm-dd to a Day -}
toDate :: String -> Day
toDate d = read d :: Day

{- Get the fraction of a year between the two input dates, t2 > t1 -}
coverage :: Day -> Day -> Double
coverage t1 t2 = (fromIntegral elapsedDays) / (fromIntegral daysInPastYear)
        where
        elapsedDays = diffDays t2 t1
        daysInPastYear = diffDays t2 $ addGregorianYearsRollOver (-1) t2

{- Compare to dates using the supplied comparison function -}
compare :: (Integer -> Integer -> Bool) -> Day -> Day -> Bool
compare f t1 t2 = f (toModifiedJulianDay t1) (toModifiedJulianDay t2)
