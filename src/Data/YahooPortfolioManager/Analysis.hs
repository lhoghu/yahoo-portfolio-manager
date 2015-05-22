module Data.YahooPortfolioManager.Analysis where

import Data.List (sort)
import Data.Time (Day, fromGregorian)
import Data.YahooPortfolioManager.DateUtils
import qualified Data.YahooPortfolioManager.DbAdapter as DB
import qualified Data.YahooPortfolioManager.TimeSeries as TS
import Data.YahooPortfolioManager.Types
import qualified Data.YahooPortfolioManager.Plot as P

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
                                (toDate (posdate pos)) 
                                ((posposition pos) + (TS.value tp))
          -- interested in the seed position = 0 here, the date is not used
          seedDate = TS.TimePoint (fromGregorian 0 0 0) 0.0

-- | Subset a list of position objects to only those that contain the 
-- input symbol
filterPositions :: Symbol -> [Position] -> [Position]
filterPositions sym = sort . filter (\p -> sym == possymbol p)

-- -- | Given and input time series of positions and and input series of
-- -- prices, compute the PV
-- pv :: TS.TimeSeries Double -> TS.TimeSeries Double -> TS.TimeSeries Double
-- pv = TS.merge (*)

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

-- | Return the number of units required to invest a given cash amount
-- at the given instrument price
computeUnits :: Double -> Double -> Double
computeUnits c p = fromIntegral . floor $ c / p

-- | The portfolio position, this records all our current holdings and their 
-- value
-- TODO might want to put this in State monad...
data Holdings = Holdings {
    cash    :: Double,
    units   :: Double,
    value   :: Double
} deriving Show

-- | The total value of all assets (cash, shares)
nav :: Holdings -> Double
nav h = (cash h) + (value h)

-- | The current pv of the position, given the stock price (not including
-- cash)
pv :: Holdings -> Double -> Double
pv h p = (units h) * p

-- | Strategy interface
class Strategy s where
    -- | Evaluate the new holdings, given a strategy, the current holdings
    -- and the current stock price
    evaluate :: s -> Holdings -> Double -> Holdings

-- | Top level, single asset, backtest function
-- Given an initial holding position (e.g. cash only) evaluate the strategy
-- given a stock price history
backtest :: (Strategy s) => s -> 
                            Holdings -> 
                            TS.TimeSeries Double -> 
                            TS.TimeSeries Holdings
backtest s h = snd . TS.mapAccum accum h
    where accum h' p = (evaluate s h' p, h' {value = pv h' p})

-- Utility to compute the new Holding given existing cash/unit positions, the
-- current stock price and the number of units in the new holding
rebalance :: Double -> Double -> Double -> Double -> Holdings
rebalance cash units price newUnits =
    Holdings (cash - (newUnits - units) * price)
             newUnits
             (newUnits * price)

-- | Buy and hold strategy
data BuyAndHold = BuyAndHold deriving Show

instance Strategy BuyAndHold where
    -- If we hold a position, do nothing, otherwise buy
    evaluate _ h p = if (units h) > 0 
                     then h 
                     else rebalance (cash h) 
                                    (units h) 
                                    p 
                                    (computeUnits (cash h) p)

-- | Strategy where we aim to maintain the pv of the stock holding within
-- some bounds. This guarantees we buy when the price is low and sell when
-- its higher (if we do buy/sell)
data ConserveValue = ConserveValue {
    minPV       :: Double,
    maxPV       :: Double,
    targetPV    :: Double
} deriving Show

instance Strategy ConserveValue where 
    -- If the value of our holdings has moved outside the bounds
    -- restore the position to match the target pv, otherwise do nothing
    evaluate s h p = if (pv h p) < (minPV s) || (pv h p) > (maxPV s)
                     then rebalance (cash h) 
                                    (units h) 
                                    p 
                                    (computeUnits (targetPV s) p)
                     else h

-- | Utility to summarise the results of a backtest
data Summary = Summary {
    finalValue      :: Double,
    finalUnits      :: Double,
    finalCash       :: Double,
    finalPV         :: Double,
    initialUnits    :: Double,
    minCash         :: Double,
    maxCash         :: Double
} deriving Show

summarise :: TS.TimeSeries Holdings -> Summary
summarise ts = Summary (nav . snd . last $ xs)
                       (units . snd . last $ xs)
                       (cash . snd . last $ xs)
                       (value . snd . last $ xs)
                       (units. snd . last . take 2 $ xs)
                       (foldl1 min $ map (cash . snd) xs)
                       (foldl1 max $ map (cash . snd) xs)
    where xs = TS.toList ts

prettySummary :: Summary -> IO ()
prettySummary s = do
    putStrLn $ (++) "Total value:\t" . show $ finalValue s
    putStrLn $ (++) "Final units:\t" . show $ finalUnits s
    putStrLn $ (++) "Final cash:\t" . show $ finalCash s
    putStrLn $ (++) "Final PV:\t" . show $ finalPV s
    putStrLn $ (++) "Initial units:\t" . show $ initialUnits s
    putStrLn $ (++) "Min cash:\t" . show $ minCash s
    putStrLn $ (++) "Max cash:\t" . show $ maxCash s

-- Test function to illustrate call:
-- Given a start date, end date and symbol, evalulate the final
-- holdings
-- Start with a simple implementation to understand the structure,
-- generalise later...
simpleStrategy :: Symbol -> Day -> Day -> IO (TS.TimeSeries Holdings)
simpleStrategy symbol start end = do
    db <- DB.dbFile    
    conn <- DB.connect db
    prices <- DB.fetchHisto conn symbol
    let s = ConserveValue 9000 11000 10000 
        h = Holdings 10000 0 0 in
        return $ backtest s h . TS.filter (TS.dateFilter start end) 
               $ prices
