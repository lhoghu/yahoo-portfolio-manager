module DbAdapter where

import Control.Monad.Trans (liftIO, MonadIO)
import Data.Conduit
import Database.HDBC
import Database.HDBC.Sqlite3
import Yahoo
import qualified Data.Conduit.List as CL

dbFile :: String
dbFile = "portfolio.db"

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
sqlCreatePortfolioTable = "create table if not exists " ++ portfolioTable ++ " (" ++
                            symbolColumn ++ " text, " ++
                            currencyColumn ++ " text, " ++
                            positionColumn ++ " real, " ++ 
                            strikeColumn ++ " real)"

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
                                priceColumn ++ " real, " ++ 
                                volumeColumn ++ " real, " ++ 
                                changeColumn ++ " real)"

{- | Define a function that recreates the db from scratch -} 
sqlCreateEntries :: IO Integer
sqlCreateEntries = do
    conn <- connectSqlite3 dbFile
    result <- run conn sqlCreatePortfolioTable []
    commit conn
    disconnect conn
    return result

class Converters a where
    toStrings :: a -> [String]
    fromSqlValues :: [SqlValue] -> a

data Portfolio = Portfolio {
    prtfsymbol      :: String,
    prtfcurrency    :: String,
    prtfposition    :: Double,
    prtfstrike      :: Double,
    prtfprice       :: Double,
    prtfchange      :: Double,
    prtfvolume      :: Double
} deriving (Show, Ord, Eq)

instance Converters Portfolio where
    toStrings p =  [ prtfsymbol p
                   , prtfcurrency p 
                   , show $ prtfposition p
                   , show $ prtfstrike p
                   , show $ prtfprice p
                   , show $ prtfchange p
                   , show $ prtfvolume p
                   ]

    fromSqlValues [sym, ccy, pos, str, pri, chg, vol] =  
        Portfolio (fromSql sym) (fromSql ccy) (fromSql pos) (fromSql str)
                  (fromSql pri) (fromSql chg) (fromSql vol)

data Position = Position {
    symbol      :: String,
    currency    :: String,
    position    :: Double,
    strike      :: Double
} deriving (Show, Ord, Eq)

instance Converters Position where
    toStrings p = [ DbAdapter.symbol p
                  , currency p 
                  , show $ position p
                  , show $ strike p
                  ]

    fromSqlValues [sym, ccy, pos, str] = Position (fromSql sym)
                                                  (fromSql ccy)
                                                  (fromSql pos)
                                                  (fromSql str)

insertPosition :: Position -> IO Integer
insertPosition p = do
    conn <- connectSqlite3 dbFile
    stmt <- prepare conn ("insert into " ++ portfolioTable ++ " values (?, ?, ?, ?)")
    result <- execute stmt [toSql (DbAdapter.symbol p),
                            toSql (currency p), 
                            toSql (position p),
                            toSql (strike p)]
    commit conn
    disconnect conn
    return result

populateQuotesTable :: [Symbol] -> IO ()
populateQuotesTable symbols = do
    sqlValues <- (getQuote symbols) $$ yahooQuoteToSqlConduit $= CL.consume
    insertYahooQuotes sqlValues

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
                    , toSql (quote q)
                    , toSql (volume q)
                    , toSql (change q)
                    ]
            yahooQuoteToSqlConduit
        Nothing -> return ()
                       
insertYahooQuotes :: [[SqlValue]] -> IO ()
insertYahooQuotes quotes = do
    conn <- connectSqlite3 dbFile
    clear <- run conn ("drop table if exists " ++ yahooQuotesTable) [] 
    create <- run conn sqlCreateYahooQuotesTable []
    stmt <- prepare conn ("insert into " ++ yahooQuotesTable ++ " values (?, ?, ?, ?)")
    res <- executeMany stmt quotes 
    commit conn
    disconnect conn
    return ()

fetchSymbols :: IO [String]
fetchSymbols = do
    conn <- connectSqlite3 dbFile
    res <- quickQuery' conn ("select distinct " ++ symbolColumn ++ " from " ++ portfolioTable) []
    disconnect conn
    return $ map (\(s:ss) -> fromSql s) res

fetchPositions :: IO [Position]
fetchPositions = do
    conn <- connectSqlite3 dbFile
    res <- quickQuery' conn ("select * from " ++ portfolioTable) []
    disconnect conn
    return $ map fromSqlValues res

fetchPortfolio :: IO [Portfolio]
fetchPortfolio = do
    conn <- connectSqlite3 dbFile
    res <- quickQuery' conn "select y.symbol, p.currency, p.position, p.strike, y.price, y.change, y.volume from yahooQuotesTable as y, portfolio as p where p.symbol = y.symbol" []
    disconnect conn
    return $ map fromSqlValues res
