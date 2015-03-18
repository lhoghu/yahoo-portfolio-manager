{-# LANGUAGE OverloadedStrings #-}

{-|
This module provides an interface to make http requests to yahoo finance
to obtain quote data on user supplied symbols.
The results are streamed back as 'YahooQuote's

See 

- <http://www.serpentine.com/wreq/tutorial.html> for usage of Network.Wreq.get
method (wreq in cabal)

- <https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview> for conduit usage
   
- <http://hackage.haskell.org/package/cassava-0.1.0.1/docs/Data-Csv.html>
for usage of Data.Csv package (cassava in cabal)
-}

module Yahoo (
    Symbol,

    -- * Types retrievable from Yahoo
    YahooQuote, LookupSymbol (..), TimePoint (..),

    -- ** @YahooQuote@ fields
    symbol, name, currency, quote, change, volume, 

    -- * Populating @YahooQuote@s
    getQuote, getHisto,

    -- * @LookupSymbol@ operations
    lookupSymbol, validateSymbol
) where

import Data.Aeson
import Data.Conduit 
import Data.Csv hiding ((.:))
import Data.List (any, intercalate)
import Data.Vector hiding ((++), mapM_)
import Data.Time (toGregorian, Day (..))
import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Control.Monad.Trans (liftIO, MonadIO)
import Network.Wreq (get, responseBody)
import Text.Printf
import Text.Regex
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

-- | Provide a Symbol type for clarity. 
-- These should be the stock symbols used by Yahoo
type Symbol = String

{-|
The type used to house the quote csv fields we get back from yahoo
Use Left Field if the conversion fails (e.g. if yahoo has no data
for that field, it might return a string, N/A, instead of a double
-}
data YahooQuote = YahooQuote {
    symbol  :: Symbol,
    name    :: String,
    currency:: String,
    quote   :: Either Field Double,
    change  :: Either Field Double,
    volume  :: Either Field Int
} deriving (Show, Eq, Ord)

{-|
The type used to contain matches for a symbol string in yahoo
-}
data LookupSymbol = LookupSymbol {
    resSymbol   :: Symbol,
    resName     :: String,
    resExch     :: String,
    resType     :: String,
    resExchDisp :: String,
    resTypeDisp :: String
} deriving (Show, Eq, Ord)

{-|
Type used to store time point for historical data
-}
data TimePoint = TimePoint {
    histoDte            :: String,
    histoOpen           :: Double,
    histoHigh           :: Double,
    histoLow            :: Double,
    histoClose          :: Double,
    histoVolume         :: Double,
    histoAdjClose       :: Double
} deriving (Show, Eq, Ord)

type Url = String
data UrlBuilder = Quote { syms :: [Symbol] } 
                | Histo { sym :: Symbol, start :: Day, end :: Day}

makeUrl' :: UrlBuilder -> Url
makeUrl' (Quote syms) = baseQuoteUri ++ (intercalate "+" syms)
makeUrl' (Histo sym start end) = printf baseHistoUri 
                                    sym
                                    fromYear (fromMonth - 1) fromDay
                                    toYear (toMonth - 1) toDay
                where
                (toYear, toMonth, toDay) = toGregorian end
                (fromYear, fromMonth, fromDay) = toGregorian start

-- The yahoo finance url template which we append with the symbols we'd like
-- quotes for
baseQuoteUri :: String
baseQuoteUri = "http://download.finance.yahoo.com/d/quotes.csv?f=snc4l1c1v&s="

-- The url template for histo data in csv format:
--      s = symbol
--      a = from month -1
--      b = from day
--      c = from year
--      d = to month -1
--      e = to day
--      f = to year
--      g = trading freq (d, w, m, or v for dividends only)
-- Returns columns:
-- Date, Open, High, Low, Close, Volume, Adj Close
-- Adj Close is a close adjusted for dividends and splits
baseHistoUri :: String
baseHistoUri = "http://ichart.yahoo.com/table.csv?s=%s&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d&g=d&ignore=.csv"

-- Provide an instance of FromRecord to decode the csv we get back from yahoo.
-- This is part of the Data.Csv module
instance FromRecord YahooQuote where
    parseRecord v
        | Data.Vector.length v == 6 = YahooQuote <$>
                            v .! 0 <*>
                            v .! 1 <*>
                            v .! 2 <*>
                            v .! 3 <*>
                            v .! 4 <*>
                            v .! 5
        | otherwise     = fail "Unable to parse Yahoo response as YahooQuote"

instance FromRecord TimePoint where
    parseRecord v
        | Data.Vector.length v == 7 = TimePoint <$>
                            v .! 0 <*>
                            v .! 1 <*>
                            v .! 2 <*>
                            v .! 3 <*>
                            v .! 4 <*>
                            v .! 5 <*>
                            v .! 6
        | otherwise     = fail "Unable to parse Yahoo response as TimePoint"

{- Remove non-utf8 chars from the bytestring. At the moment the £ is
 - the only known problem
 -}
removeUnwantedChars :: BSL.ByteString -> BSL.ByteString
removeUnwantedChars = BSL.concat . BS.split '\xa3'

{-| Make an http request to yahoo for stock symbols and stream the result back.
The list of @Symbol@s are passed to the http request, therefore should be
recognised by yahoo.

See 'Lifting Operations' in conduit documentation for orientation on how to 
process the resulting stream.
-}
getUrl :: (MonadIO m, FromRecord a) => UrlBuilder -> Source m a
getUrl u = do
    r <- liftIO $ get (makeUrl' u)
    case Data.Csv.decode NoHeader (removeUnwantedChars $ r ^. responseBody) of
        Left err -> liftIO $ putStrLn ("Failed to decode yahoo response: " ++ err)
        Right vals -> mapM_ yield (toList vals)

getQuote :: MonadIO m => [Symbol] -> Source m YahooQuote
getQuote symbols = do getUrl $ Quote symbols

{-| Retrieve historical time series from Yahoo
 -}
getHisto :: MonadIO m => Symbol -> Day -> Day -> Source m TimePoint
getHisto symbol from to = do getUrl $ Histo symbol to from

makeLookupUrl :: Symbol -> String
makeLookupUrl s = "http://d.yimg.com/autoc.finance.yahoo.com/autoc?query=" ++
                    s ++ "&callback=YAHOO.Finance.SymbolSuggest.ssCallback";

-- Yahoo returns a string with a json string enclosed within a surrounding
-- class method. Here we return the inner json string
stripClassname :: String -> String
stripClassname s = case matchRegexAll (mkRegex "YAHOO\\.Finance\\.SymbolSuggest\\.ssCallback\\((.*?)\\)") s of
                    Just (_, _, _, match:_) -> match
                    _ -> fail "Failed to parse yahoo lookup result: " ++ s

-- Haskell type representation of the yahoo JSON response
data LookupJsonResponse = LookupJsonResponse { 
    resultSet :: ResultSet
} deriving (Show)

data ResultSet = ResultSet {
    -- query :: String,
    result :: [LookupSymbol]
} deriving (Show)

-- FromJSON instances for the Aeson decoder
instance FromJSON LookupJsonResponse where
    parseJSON (Object v) = LookupJsonResponse <$> v .: "ResultSet"
    parseJSON _ = mzero

instance FromJSON ResultSet where
    parseJSON (Object v) = ResultSet <$> v .: "Result"
    parseJSON _ = mzero

instance FromJSON LookupSymbol where
    parseJSON (Object v) = LookupSymbol <$> 
                                v .: "symbol" <*> 
                                v .: "name" <*>
                                v .: "exch" <*>
                                v .: "type" <*>
                                v .: "exchDisp" <*>
                                v .: "typeDisp"
    parseJSON _ = mzero

{-| Lookup a symbol or partial symbol in yahoo and return list of matches.
    Yahoo will return up to 10 matches
-}
lookupSymbol :: MonadIO m => Symbol -> Source m LookupSymbol
lookupSymbol sym = do
    r <- liftIO $ get (makeLookupUrl sym)
    let jsonByteString = BS.pack $ stripClassname (BS.unpack $ r ^. responseBody)
    case Data.Aeson.decode jsonByteString of
        Just res -> mapM_ yield (result . resultSet $ res)
        _ -> liftIO $ fail "Failed to decode yahoo json response"

validateSymbol :: MonadIO m => Symbol -> m Bool
validateSymbol s = do
    r <- lookupSymbol s $$ CL.consume
    liftIO $ return (matchSymbol s r)

matchSymbol :: String -> [LookupSymbol] -> Bool
matchSymbol x = Data.List.any (\s -> ((resSymbol s) == x))

-- Test code to see yahoo quote stream in ghci
--
-- source :: Source IO Int -- produces a stream of Ints
-- source = CL.sourceList [1..4]
-- 
-- sink :: Sink String IO () -- consumes a stream of Strings, no result
-- sink = CL.mapM_ putStrLn
-- 
-- conduit :: Conduit YahooQuote IO String -- converts YahooQuotes into Strings
-- conduit = CL.map show
-- 
-- testSymbols = ["GOOG", "AAPL"]
-- testResult = (getQuote testSymbols) $$ conduit $= sink
-- testLookup = (lookupSymbol "IGL") $$ conduit $= sink
