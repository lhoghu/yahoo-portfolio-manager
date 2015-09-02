module Data.YahooPortfolioManager 
    ( 
    --   module Data.YahooPortfolioManager.DateUtils
    -- , module Data.YahooPortfolioManager.DbAdapter
    -- , module Data.YahooPortfolioManager.TimeSeries
    -- , module Data.YahooPortfolioManager.Types
    -- , module Data.YahooPortfolioManager.Yahoo
    )
    where

import Data.YahooPortfolioManager.DateUtils 
import Data.YahooPortfolioManager.DbAdapter
-- import Data.YahooPortfolioManager.TimeSeries
-- import Data.YahooPortfolioManager.Types
-- import Data.YahooPortfolioManager.Yahoo

import Database.HDBC (IConnection)

{- Update historical quotes based on symbols in the db (as returned by 
 - fetchSymbols) using data from yahoo
 -}
updateHistoQuotes :: IConnection conn => conn -> IO ()
updateHistoQuotes conn = do
    today <- getDate
    symbols <- fetchSymbols conn 
    mapM_ (update today) symbols    
    where update t s = populateHistoQuotes conn s (toDate "2001-01-01") t
