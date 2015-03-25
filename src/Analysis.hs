module Analysis where

import Data.List (sort)
import Data.Time (Day, fromGregorian)
import DateUtils
import qualified TimeSeries as TS
import Types

-- | Create a time series of positions given a list of positions as stored
-- in the db, and a list of dates used to construct the date index of the
-- time series. The list of positions is filtered to contain only positions
-- relevant for the input symbol
positionTimeSeries :: [Day] -> Symbol -> [Position] -> TS.TimeSeries Double
positionTimeSeries ds s = interpolate ds 0.0 
                          . TS.create . positionsToTimePoints 
                          . filterPositions s

-- | Perform step-wise interpolation between time points
-- The input value a is used to fill in values in the [Day] list
-- earlier than any dates in the time series. Constant extrapolation
-- is used after the final date in the time series
interpolate :: [Day] -> a -> TS.TimeSeries a -> TS.TimeSeries a
interpolate []     _ ts = ts
interpolate (d:ds) v ts = case TS.fetch d ts of
    Just tp -> interpolate ds (TS.value tp) ts
    Nothing -> interpolate ds v $ TS.insert (TS.TimePoint d v) ts

-- | When we create the list of time points, we have to accumulate the total
-- position at time t, since the positions list stores the history of
-- transactions (therefore the position it stores is the change of the total
-- position)
positionsToTimePoints :: [Position] -> [TS.TimePoint Double]
positionsToTimePoints = scanl toTimePoint seedDate
    where toTimePoint tp pos = TS.TimePoint 
                                (toDate (date pos)) 
                                ((position pos) + (TS.value tp))
          -- interested in the seed position = 0 here, the date is not used
          seedDate = TS.TimePoint (fromGregorian 0 0 0) 0.0

-- | Subset a list of position objects to only those that contain the 
-- input symbol
filterPositions :: Symbol -> [Position] -> [Position]
filterPositions sym = sort . filter (\p -> sym == symbol p)

-- | Given and input time series of positions and and input series of
-- prices, compute the PV
pv :: TS.TimeSeries Double -> TS.TimeSeries Double -> TS.TimeSeries Double
pv = TS.merge (*)

-- | Arithmetic mean of the input time series values
mean :: Fractional a => TS.TimeSeries a -> a
mean ts = TS.foldl (+) 0.0 ts / denominator
    where denominator = fromIntegral $ TS.length ts

-- | Standard deviation (biased) of the input time series
stddev :: Floating a => TS.TimeSeries a -> a
stddev ts = sqrt $ sumsquares / denominator
    where sumsquares  = TS.foldl (\s v -> s + (v - x) * (v - x)) 0.0 ts
          denominator = fromIntegral $ TS.length ts
          x           = mean ts
