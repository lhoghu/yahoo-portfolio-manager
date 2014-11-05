module DbAdapter where

import Control.Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Conduit
import Database.HDBC
import Database.HDBC.Sqlite3
import Paths_yahoo_portfolio_manager
import Text.Printf
import Yahoo
import qualified Data.Conduit.List as CL

dbFile :: IO FilePath
dbFile = getDataFileName "portfolio.db"

--------------------------------------------------------------------------------

portfolioTable :: String
portfolioTable = "portfolio"

symbolColumn :: String
symbolColumn = "symbol"

currencyColumn :: String
currencyColumn = "currency"

positionColumn :: String
positionColumn = "position"

strikeColumn :: String
strikeColumn = "strike"

sqlCreatePortfolioTable :: String
sqlCreatePortfolioTable = "create table if not exists " ++ 
    portfolioTable ++ " (" ++ symbolColumn ++ " text, " ++
                              currencyColumn ++ " text, " ++
                              positionColumn ++ " real, " ++ 
                              strikeColumn ++ " real)"

--------------------------------------------------------------------------------

yahooQuotesTable :: String
yahooQuotesTable = "yahooQuotesTable"

priceColumn :: String
priceColumn = "price"

changeColumn :: String
changeColumn = "change"

volumeColumn :: String
volumeColumn = "volume"

sqlCreateYahooQuotesTable :: String
sqlCreateYahooQuotesTable = "create table if not exists " ++ 
    yahooQuotesTable ++ " (" ++ symbolColumn ++ " text, " ++ 
                                currencyColumn ++ " text, " ++ 
                                priceColumn ++ " real, " ++ 
                                volumeColumn ++ " real, " ++ 
                                changeColumn ++ " real)"

--------------------------------------------------------------------------------

fxTable :: String
fxTable = "fx"

fromCcyColumn :: String
fromCcyColumn = "fromCcy"

toCcyColumn :: String
toCcyColumn = "toCcy"

fxColumn :: String
fxColumn = "fx"

sqlCreateFxTable :: String
sqlCreateFxTable = "create table if not exists " ++
    fxTable ++ " (" ++ toCcyColumn ++ " text, " ++
                       fromCcyColumn ++ " text, " ++
                       fxColumn ++ " real)"

--------------------------------------------------------------------------------

connect :: FilePath -> IO Connection
connect fp = do
    conn <- connectSqlite3 fp
    sqlCreateEntries conn
    return conn

--------------------------------------------------------------------------------

{- | Define a function that recreates the db from scratch -} 
sqlCreateEntries :: IConnection conn => conn -> IO ()
sqlCreateEntries conn = do
    tables <- getTables conn
    when (not (portfolioTable `elem` tables)) $
        do 
            run conn sqlCreatePortfolioTable []
            return ()
    commit conn

--------------------------------------------------------------------------------

class Converters a where
    toStrings :: a -> [String]
    fromSqlValues :: [SqlValue] -> a
    toSqlValues :: a -> [SqlValue]

headers :: [String]
headers = ["Sym", "Ccy", "Pos", "@", "Quote", "Chg", "(%)", "Vol", "FX"]

data Portfolio = Portfolio {
    prtfsymbol      :: String,
    prtfcurrency    :: String,
    prtfposition    :: Double,
    prtfstrike      :: Double,
    prtfprice       :: Double,
    prtfchange      :: Double,
    prtfpctchange   :: Double,
    prtfvolume      :: Double,
    prtffxp         :: Double
} deriving (Show, Ord, Eq)

instance Converters Portfolio where
    toStrings p =  [ prtfsymbol p
                   , prtfcurrency p 
                   , show $ prtfposition p
                   , show $ prtfstrike p
                   , printf "%.2f" (prtfprice p)
                   , printf "%.2f" (prtfchange p)
                   , printf "%.2f" (100.0 * prtfpctchange p)
                   , printf "%.0f" (prtfvolume p)
                   , show $ prtffxp p
                   ]

    fromSqlValues [sym, ccy, pos, str, pri, chg, pch, vol, fxp] =  
        Portfolio (fromSql sym) (fromSql ccy) (fromSql pos) (fromSql str)
                  (fromSql pri) (fromSql chg) (fromSql pch) (fromSql vol) 
                  (fromSql fxp)

    toSqlValues (Portfolio sym ccy pos str pri chg pch vol fxp) =  
        [toSql sym, toSql ccy, toSql pos, toSql str, toSql pri, toSql chg, 
         toSql pch, toSql vol, toSql fxp]

data Position = Position {
    symbol      :: String,
    currency    :: String,
    position    :: Double,
    strike      :: Double
} deriving (Show, Ord, Eq)

instance Converters Position where
    toStrings p = [ DbAdapter.symbol p
                  , DbAdapter.currency p 
                  , show $ position p
                  , show $ strike p
                  ]

    fromSqlValues [sym, ccy, pos, str] = Position (fromSql sym)
                                                  (fromSql ccy)
                                                  (fromSql pos)
                                                  (fromSql str)

    toSqlValues (Position sym ccy pos str) =
        [toSql sym, toSql ccy, toSql pos, toSql str]

data Fx = Fx {
    fxToCcy :: String,
    fxFromCcy :: String,
    fx :: Double
} deriving (Show, Ord, Eq)

instance Converters Fx where
    toStrings f = [fxToCcy f, fxFromCcy f, show $ fx f]
    fromSqlValues [to, from, f] = Fx (fromSql to) (fromSql from) (fromSql f)
    toSqlValues (Fx to from f) = [toSql to, toSql from, toSql f]

--------------------------------------------------------------------------------

insertPosition :: IConnection conn => conn -> Position -> IO Integer
insertPosition conn p = do
    stmt <- prepare conn ("insert into " ++ portfolioTable ++ " values (?, ?, ?, ?)")
    result <- execute stmt (toSqlValues p)
    commit conn
    return result

insertFx :: IConnection conn => conn -> [Fx] -> IO ()
insertFx conn fxs = do
    clear <- run conn ("drop table if exists " ++ fxTable) [] 
    create <- run conn sqlCreateFxTable []
    stmt <- prepare conn ("insert into " ++ fxTable ++ " values (?, ?, ?)")
    res <- executeMany stmt (map toSqlValues fxs)
    commit conn
    return ()

populateQuotesTable :: IConnection conn => conn -> [Symbol] -> IO ()
populateQuotesTable conn symbols = do
    sqlValues <- (getQuote symbols) $$ yahooQuoteToSqlConduit $= CL.consume
    insertYahooQuotes conn sqlValues

{- | Define a conduit to transform the stream of yahoo quotes into
 - a stream of SqlValue lists, which, when consumed by a sink is
 - ready for insertion in the db
 -}
yahooQuoteToSqlConduit :: Conduit YahooQuote IO [SqlValue]
yahooQuoteToSqlConduit = do
    quoteStream <- await
    case quoteStream of
        Just q -> do
            yield $ [ toSql (Yahoo.symbol q) 
                    , toSql (Yahoo.currency q)
                    , toSql (quote q)
                    , toSql (volume q)
                    , toSql (change q)
                    ]
            yahooQuoteToSqlConduit
        Nothing -> return ()
                       
insertYahooQuotes :: IConnection conn => conn -> [[SqlValue]] -> IO ()
insertYahooQuotes conn quotes = do
    clear <- run conn ("drop table if exists " ++ yahooQuotesTable) [] 
    create <- run conn sqlCreateYahooQuotesTable []
    stmt <- prepare conn ("insert into " ++ yahooQuotesTable ++ " values (?, ?, ?, ?, ?)")
    res <- executeMany stmt quotes 
    commit conn
    return ()

--------------------------------------------------------------------------------

fetchSymbols :: IConnection conn => conn -> IO [String]
fetchSymbols conn = do
    res <- quickQuery' conn ("select distinct " ++ symbolColumn ++ " from " ++ portfolioTable) []
    return $ map (\(s:ss) -> fromSql s) res

fetchFx :: IConnection conn => conn -> IO [Fx]
fetchFx conn = do
    res <- quickQuery' conn ("select distinct p.currency, y.currency, 1.0 from yahooQuotesTable as y, portfolio as p where p.symbol = y.symbol") []
    return $ map fromSqlValues res

fetchPositions :: IConnection conn => conn -> IO [Position]
fetchPositions conn = do
    res <- quickQuery' conn ("select * from " ++ portfolioTable) []
    return $ map fromSqlValues res

fetchPortfolio :: IConnection conn => conn -> IO [Portfolio]
fetchPortfolio conn = do
    res <- quickQuery' conn "select y.symbol, p.currency, p.position, p.strike, y.price, y.change, y.change / y.price, y.volume, f.fx from yahooQuotesTable as y, portfolio as p, fx as f where p.symbol = y.symbol and f.toCcy = p.currency and f.fromCcy = y.currency" []
    return $ map fromSqlValues res
