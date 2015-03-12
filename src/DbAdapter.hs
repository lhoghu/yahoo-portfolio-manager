{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Implementation of persistent db storage for user defined portfolio information
and quote information retrieved from yahoo
-}
module DbAdapter (
    dbFile, connect,

    stringify, toStrings,

    -- ** Types queryable in the db
    Portfolio(..), Position(..), Dividend(..), Fx(..),

    -- ** Db updates
    insertPosition, insertDividend, insertFx, populateQuotesTable, 
    populateHistoQuotes,

    -- ** Db retrieval
    fetchSymbols, fetchPositions, fetchFx, fetchPortfolio, fetchDividends
) where

import Control.Monad (when)
import Data.Conduit (($=), ($$), await, yield, Conduit (..))
import Data.Convertible
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time (Day (..))
import Paths_yahoo_portfolio_manager
import System.Directory (createDirectoryIfMissing)
import Text.Printf
import Yahoo
import qualified Data.Conduit.List as CL

-- | The location of the db on disk
dbFile :: IO FilePath
dbFile = do
    dataDir <- getDataDir
    _ <- createDirectoryIfMissing True dataDir
    getDataFileName "portfolio.db"

-- The location of sql queries on disk
portfolioSqlFile :: IO FilePath
portfolioSqlFile = getDataFileName "src/sql/portfolio.sql"

--------------------------------------------------------------------------------

{- Strings for the portfolio data. This is populated with the (yahoo) symbols
 the user holds a postition in, the number of units they own and the
 price they bought them at
 -}
portfolioTable :: String
portfolioTable = "portfolio"

symbolColumn :: String
symbolColumn = "symbol"

currencyColumn :: String
currencyColumn = "currency"

dateColumn :: String
dateColumn = "date"

positionColumn :: String
positionColumn = "position"

strikeColumn :: String
strikeColumn = "strike"

-- Sql to create the portfolio table in the db
sqlCreatePortfolioTable :: String
sqlCreatePortfolioTable = "create table if not exists " ++ 
    portfolioTable ++ " (" ++ symbolColumn ++ " text, " ++
                              currencyColumn ++ " text, " ++
                              dateColumn ++ " text, " ++ 
                              positionColumn ++ " real, " ++ 
                              strikeColumn ++ " real)"

--------------------------------------------------------------------------------

{- Strings for the table populated with quotes data pulled from yahoo finance.
In addition to the portfolio table info, this contains the latest price in 
yahoo, the price move since open and the volume
-}
yahooQuotesTable :: String
yahooQuotesTable = "yahooQuotesTable"

priceColumn :: String
priceColumn = "price"

changeColumn :: String
changeColumn = "change"

volumeColumn :: String
volumeColumn = "volume"

-- Sql to create the yahoo quotes table in the db
sqlCreateYahooQuotesTable :: String
sqlCreateYahooQuotesTable = "create table if not exists " ++ 
    yahooQuotesTable ++ " (" ++ symbolColumn ++ " text, " ++ 
                                currencyColumn ++ " text, " ++ 
                                priceColumn ++ " real, " ++ 
                                volumeColumn ++ " real, " ++ 
                                changeColumn ++ " real)"

--------------------------------------------------------------------------------

{- Strings for the table of historical quote data pulled from Yahoo finance.
 -}
yahooHistoQuotes :: String
yahooHistoQuotes = "yahooHistoQuotes"

histoSymbolCol :: String
histoSymbolCol = "symbol"

histoDateCol :: String
histoDateCol = "date"

histoOpenCol :: String
histoOpenCol = "open"

histoHighCol :: String
histoHighCol = "high"

histoLowCol :: String
histoLowCol = "low"

histoCloseCol :: String
histoCloseCol = "close"

histoVolumeCol :: String
histoVolumeCol = "volume"

histoAdjCloseCol :: String
histoAdjCloseCol = "adjClose"

sqlCreateYahooHistoTable = "create table if not exists " ++ 
    yahooHistoQuotes ++ " (" ++ histoSymbolCol ++ " text, " ++
                                histoDateCol ++ " text, " ++
                                histoOpenCol ++ " real, " ++ 
                                histoHighCol ++ " real, " ++
                                histoLowCol ++ " real, " ++ 
                                histoCloseCol ++ " real, " ++
                                histoVolumeCol ++ " real, " ++
                                histoAdjCloseCol ++ " real)"

--------------------------------------------------------------------------------

-- Strings for the table populated with fx data pulled from yahoo
fxTable :: String
fxTable = "fx"

fromCcyColumn :: String
fromCcyColumn = "fromCcy"

toCcyColumn :: String
toCcyColumn = "toCcy"

fxColumn :: String
fxColumn = "fx"

prtfFxColumn :: String
prtfFxColumn = "prtfFx"

-- Sql to create the fx table in the db
sqlCreateFxTable :: String
sqlCreateFxTable = "create table if not exists " ++
    fxTable ++ " (" ++ toCcyColumn ++ " text, " ++
                       fromCcyColumn ++ " text, " ++
                       fxColumn ++ " real, " ++
                       prtfFxColumn ++ ")"

--------------------------------------------------------------------------------

{- Strings for the table populated with dividend information provided by the user.
-}
dividendsTable :: String
dividendsTable = "dividendsTable"

divSymbolColumn :: String
divSymbolColumn = "symbol"

dividendColumn :: String
dividendColumn = "dividend"

divDateColumn :: String
divDateColumn = "date"

-- Sql to create the dividends table in the db
sqlCreateDividendsTable :: String
sqlCreateDividendsTable = "create table if not exists " ++ 
    dividendsTable ++ " (" ++ divSymbolColumn ++ " text, " ++ 
                                dividendColumn ++ " real, " ++ 
                                divDateColumn ++ " text)"

--------------------------------------------------------------------------------

-- | Initialise the db with a minimal schema and return a db connection.
-- The db will be created if it doesn't already exist, provided the 
-- parent folder exists
connect :: FilePath -> IO Connection
connect fp = 
    handleSql errorHandler $
    do
        conn <- connectSqlite3 fp
        sqlCreateEntries conn
        return conn
    where errorHandler e = 
            do fail $ "Error connecting to database (" ++ show fp ++ "): " 
                        ++ show e

--------------------------------------------------------------------------------

{- Ensure the db exists with a minimal schema
Also perform any checks on db integrity here
-} 
sqlCreateEntries :: IConnection conn => conn -> IO ()
sqlCreateEntries conn = do
    tables <- getTables conn
    when (or [not (portfolioTable `elem` tables),
          not (dividendsTable `elem` tables)]) $
        do 
            run conn sqlCreatePortfolioTable []
            run conn sqlCreateDividendsTable []
            return ()
    commit conn

--------------------------------------------------------------------------------

{- 
Convert between haskell data types and sql tables for ease of
db insertion and retrieval
-}
class Converters a where
    toStrings :: a -> [String]
    fromSqlValues :: [SqlValue] -> a
    toSqlValues :: a -> [SqlValue]


{- | Haskell structure that represents the command line output table
  including yahoo quotes and portfolio position info
 -}
data Portfolio = Portfolio {
    prtfunits       :: Double,
    prtfprice       :: Double,
    prtfdiv         :: Maybe Double,
    prtfsymbol      :: String,
    prtfcost        :: Double,
    prtfcurrent     :: Maybe Double,
    prtfchange      :: Maybe Double,
    prtfpctchange   :: Maybe Double,
    prtfpnl         :: Maybe Double,
    prtfpctpnl      :: Maybe Double
} deriving (Show, Ord, Eq)

instance Converters Portfolio where
    toStrings p =  [ prtfsymbol p
                   , printf "%.0f" (prtfunits p)
                   , printf "%.2f" (prtfprice p)
                   , printf "%.2f" (prtfcost p)
                   , printf "%.2f" (handleNull (prtfcurrent p))
                   , printf "%.2f" (handleNull (prtfchange p))
                   , printf "%.2f" (100.0 * (handleNull (prtfpctchange p)))
                   , printf "%.2f" (handleNull (prtfdiv p))
                   , printf "%.2f" (handleNull (prtfpnl p))
                   , printf "%.2f" (100.0 * (handleNull (prtfpctpnl p)))
                   ]
                   where
                   handleNull (Just d) = d
                   handleNull Nothing = 0.0

    fromSqlValues [pos, pri, div, sym, str, cur, chg, pch, pnl, plc] =  
        Portfolio (fromSql pos) (fromSql pri) (fromSql div) 
                  (fromSql sym) (fromSql str) 
                  (fromSql cur) (fromSql chg) (fromSql pch) 
                  (fromSql pnl) (fromSql plc)

    toSqlValues (Portfolio pos pri div sym str cos chg pch pnl plc) =  
        [toSql pos, toSql pri, toSql sym, toSql div, toSql str, toSql cos, 
         toSql chg, toSql pch, toSql pnl, toSql plc]

{-| Haskell structure that contains the portfolio position information
  entered by the user
 -}
data Position = Position {
    symbol      :: String,
    currency    :: String,
    date        :: String,
    position    :: Double,
    strike      :: Double
} deriving (Show, Ord, Eq)

instance Converters Position where
    toStrings p = [ DbAdapter.symbol p
                  , DbAdapter.currency p 
                  , DbAdapter.date p 
                  , show $ position p
                  , show $ strike p
                  ]

    fromSqlValues [sym, ccy, dte, pos, str] = Position (fromSql sym)
                                                          (fromSql ccy)
                                                          (fromSql dte)
                                                          (fromSql pos)
                                                          (fromSql str)

    toSqlValues (Position sym ccy dte pos str) =
        [toSql sym, toSql ccy, toSql dte, toSql pos, toSql str]

{-| Haskell structure that contains the fx to convert yahoo quotes into
 the symbol currency entered by the user
 -}
data Fx = Fx {
    fxToCcy :: String,
    fxFromCcy :: String,
    fx :: Double,
    prtfFx :: Double
} deriving (Show, Ord, Eq)

instance Converters Fx where
    toStrings f = [fxToCcy f, fxFromCcy f, show $ fx f, show $ prtfFx f]
    fromSqlValues [to, from, f, pf] = Fx (fromSql to) 
                                         (fromSql from) 
                                         (fromSql f) 
                                         (fromSql pf)
    toSqlValues (Fx to from f pf) = [toSql to, toSql from, toSql f, toSql pf]

{-| Haskell structure that contains the portfolio position information
  entered by the user
 -}
data Dividend = Dividend {
    divsymbol   :: String,
    dividend    :: Double,
    divdate     :: String
} deriving (Show, Ord, Eq)

instance Converters Dividend where
    toStrings d = [ divsymbol d
                  , show $ dividend d 
                  , divdate d
                  ]

    fromSqlValues [sym, div, dte] = Dividend (fromSql sym)
                                             (fromSql div)
                                             (fromSql dte)

    toSqlValues (Dividend sym div dte) =
        [toSql sym, toSql div, toSql dte]

{- |
Convert a list of Converters instances into a list of strings.
Each converter object has its fields converted to string representations.
The list of fields is further converted into a single string using the supplied
([String] -> String) function
-}
stringify :: Converters a => ([String] -> String) -> [a] -> [String]
stringify f = map (f. toStrings)

--------------------------------------------------------------------------------

{-| Add a new position to the db

The user specifies all the fields to be inserted in the db.
-}
insertPosition :: IConnection conn => conn -> Position -> IO Integer
insertPosition conn p =
    handleSql errorHandler $
    do
        stmt <- prepare conn ("insert into " ++ portfolioTable 
                                ++ " values (?, ?, ?, ?, ?)")
        result <- execute stmt (toSqlValues p)
        commit conn
        return result
    where 
    errorHandler e = do
        fail $ "Failed to add position " ++ show p ++ " to db: " ++ show e

{-| Add a new dividend payment to the db

The user specifies all the fields to be inserted in the db.
-}
insertDividend :: IConnection conn => conn -> Dividend -> IO Integer
insertDividend conn div =
    handleSql errorHandler $
    do
        stmt <- prepare conn ("insert into " ++ dividendsTable 
                                ++ " values (?, ?, ?)")
        result <- execute stmt (toSqlValues div)
        commit conn
        return result
    where 
    errorHandler e = do
        fail $ "Failed to add dividend " ++ show div ++ " to db: " ++ show e

{-| Add the current fx to the db

The required fxs are inferred from the currencies the user has specified
of the price they entered a position in, and the currency yahoo claims it
returns quotes in.
There are known issues with the yahoo currency so the retrieval of the
appropriate fx values from yahoo is not implemented yet
-}
insertFx :: IConnection conn => conn -> [Fx] -> IO ()
insertFx conn fxs = 
    handleSql errorHandler $
    do
        run conn ("drop table if exists " ++ fxTable) [] 
        run conn sqlCreateFxTable []
        stmt <- prepare conn ("insert into " ++ fxTable ++ " values (?, ?, ?, ?)")
        executeMany stmt (map toSqlValues fxs)
        commit conn
        return ()
    where 
    errorHandler e = do
        fail $ "Failed to add fx to db: " ++ show e

{-| Add yahoo quotes to the db

The set of symbols to retrieve is inferred from the symbols the user
has entered in their portfolio.
-}
populateQuotesTable :: IConnection conn => conn -> [Symbol] -> IO ()
populateQuotesTable conn symbols = do
    sqlValues <- (getQuote symbols) $$ yahooQuoteToSqlConduit $= CL.consume
    insertYahooQuotes conn sqlValues

{- Define a convertible instance to translate Either results from the 
 - Yahoo query into Maybe results
 -}
instance (Convertible b SqlValue) => Convertible (Either a b) SqlValue where
    safeConvert (Left _) = return SqlNull
    safeConvert (Right b) = safeConvert (Just b)

{- Define a conduit to transform the stream of yahoo quotes into
  a stream of SqlValue lists, which, when consumed by a sink is
  ready for insertion in the db
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
insertYahooQuotes conn quotes = 
    handleSql errorHandler $
    do
        run conn ("drop table if exists " ++ yahooQuotesTable) [] 
        run conn sqlCreateYahooQuotesTable []
        stmt <- prepare conn ("insert into " ++ yahooQuotesTable ++ " values (?, ?, ?, ?, ?)")
        executeMany stmt quotes 
        commit conn
        return ()
    where 
    errorHandler e = do
        fail $ "Failed to ad yahoo quotes to db: " ++ show e

{- Add historical yahoo quotes to the db
 -}
populateHistoQuotes :: IConnection conn => conn -> Symbol -> Day -> Day -> IO ()
populateHistoQuotes conn symbol from to = do
    sqlValues <- (getHisto symbol from to) 
                    $$ yahooHistoToSqlConduit symbol
                    $= CL.consume
    insertYahooHistoQuotes conn sqlValues

yahooHistoToSqlConduit :: Symbol -> Conduit TimePoint IO [SqlValue]
yahooHistoToSqlConduit symbol = do
    quoteStream <- await
    case quoteStream of
        Just q -> do
            yield $ [ toSql symbol
                    , toSql (Yahoo.histoDte q)
                    , toSql (Yahoo.histoOpen q)
                    , toSql (Yahoo.histoHigh q)
                    , toSql (Yahoo.histoLow q)
                    , toSql (Yahoo.histoClose q)
                    , toSql (Yahoo.histoVolume q)
                    , toSql (Yahoo.histoAdjClose q)
                    ]
            yahooHistoToSqlConduit symbol
        Nothing -> return ()

insertYahooHistoQuotes :: IConnection conn => conn -> [[SqlValue]] -> IO ()
insertYahooHistoQuotes conn quotes = 
    handleSql errorHandler $
    do
        run conn sqlCreateYahooHistoTable []
        stmt <- prepare conn ("insert into " ++ yahooHistoQuotes ++ 
                                " values (?, ?, ?, ?, ?, ?, ?, ?)")
        executeMany stmt quotes
        commit conn
        return ()
    where
    errorHandler e = do
        fail $ "Failed to insert historical Yahoo quotes in the db: " ++ show e

--------------------------------------------------------------------------------

{-| Retrieve symbols from the db

This is the set of unique symbols specified by the user as part of their
portfolio
-}
fetchSymbols :: IConnection conn => conn -> IO [String]
fetchSymbols conn = 
    handleSql errorHandler $
    do
        res <- quickQuery' conn ("select distinct " ++ symbolColumn 
                                    ++ " from " ++ portfolioTable) []
        return $ map (\(s:_) -> fromSql s) res
    where 
    errorHandler e = do
        fail $ "Failed to fetch symbols from db: " ++ show e

{-| Fetch fx from the db

This is the set of fxs we require based on the user defined and yahoo 
currencies in the portfolio. 
-}
fetchFx :: IConnection conn => conn -> IO [Fx]
fetchFx conn = 
    handleSql errorHandler $
    do
        res <- quickQuery' conn ("select distinct p.currency, y.currency, 1.0, 1.0 " 
                                    ++ "from yahooQuotesTable as y, " 
                                    ++ "portfolio as p "
                                    ++ "where p.symbol = y.symbol") []
        return $ map fromSqlValues res
    where 
    errorHandler e = do
        fail $ "Failed to fetch fx from db: " ++ show e

{-| Fetch the user defined positions from the db -}
fetchPositions :: IConnection conn => conn -> IO [Position]
fetchPositions conn =
    handleSql errorHandler $
    do
        res <- quickQuery' conn ("select * from " ++ portfolioTable) []
        return $ map fromSqlValues res
    where
    errorHandler e = do
        fail $ "Failed to fetch positions from db: " ++ show e

{-| Fetch the user defined dividends from the db -}
fetchDividends :: IConnection conn => conn -> IO [Dividend]
fetchDividends conn =
    handleSql errorHandler $
    do
        res <- quickQuery' conn ("select * from " ++ dividendsTable) []
        return $ map fromSqlValues res
    where
    errorHandler e = do
        fail $ "Failed to fetch dividends from db: " ++ show e

{-| Fetch the portfolio from the db

This is the set of positions defined by the user, supplemented with 
the latest quote data from yahoo
-}
fetchPortfolio :: IConnection conn => conn -> IO [Portfolio]
fetchPortfolio conn =
    handleSql errorHandler $
    do
        fn <- portfolioSqlFile
        sqlQuery <- readFile fn
        _ <- run conn sqlQuery []
        res <- quickQuery' conn 
                "select * from \
                \(\
                \        select * from daily_pnl\
                \        union select\
                \                0.0,\
                \                0.0,\
                \                sum(dividends),\
                \                'TOTAL',\
                \                sum(cost),\
                \                sum(current_value),\
                \                sum(change),\
                \                sum(change) / sum(current_value),\
                \                sum(total_change),\
                \                sum(current_value - cost +dividends)/sum(cost)\
                \        from daily_pnl\
                \) as t\
                \ order by case t.symbol when 'TOTAL' then 100 else 1 end" []
        return $ map fromSqlValues res
    where 
    errorHandler e = do
        fail $ "Failed to fetch portfolio from db: " ++ show e
