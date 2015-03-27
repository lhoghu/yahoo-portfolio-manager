module Plot where

import Data.Time
import DateUtils
import DbAdapter
--import System.Exit
import qualified TimeSeries as TS
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Simple as GS
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Time (prepXTime)

import Database.HDBC.Sqlite3

symbol :: String
symbol = "IWRD.L"

mydata :: (TS.TimePoint Double -> Bool) -> TS.TimeSeries Double -> Plot2D.T Int Double
mydata filter ts = Plot2D.list Graph2D.listPoints . tsValues $ TS.filter filter ts

dateFilter :: Day -> Day -> TS.TimePoint a -> Bool
dateFilter from to tp = (TS.date tp) >= from && (TS.date tp) <= to

test1 :: IO ()
test1 = do
    conn <- connectSqlite3 "portfolio.db"
    ts <- fetchHisto conn symbol
    GS.plotList [] (TS.toList ts)

test2 :: IO () --System.Exit.ExitCode
test2 = do
    --db <- dbFile
    conn <- connectSqlite3 "portfolio.db" --db
    ts <- fetchHisto conn symbol
    end <- getDate
    let start = addGregorianYearsRollOver (-5) end in do
        sequence_ $ GP.plotDefault (mydata (dateFilter start end) ts) : []

plotSymbol :: String -> IO ()
plotSymbol s = do
    conn <- connectSqlite3 "portfolio.db"
    ts <- fetchHisto conn s
    -- can also try plotDots with sim effect
    -- can specify angle and offset in rotate: rotate by -60 offset -0.5, -1.5
    -- ... these attributes really do just wrap the gnuplot commands
    GS.plotPath [GS.Title s, GS.Key Nothing, GS.XTicks (Just ["rotate"]), GS.XTime, GS.XFormat "%Y-%m-%d"] 
        $ prepXTime 
        $ map (\(d, v) -> (UTCTime d 0, v)) (TS.toList ts) 

tsValues :: TS.TimeSeries a -> [a]
tsValues = map snd . TS.toList

tsDates :: TS.TimeSeries a -> [Day]
tsDates = map fst . TS.toList
