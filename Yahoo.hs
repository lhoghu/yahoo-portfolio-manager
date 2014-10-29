{-# LANGUAGE OverloadedStrings #-}

{- Yahoo.hs
 -
 - See http://www.serpentine.com/wreq/tutorial.html for usage of Network.Wreq.get
 - method (wreq in cabal)
 -
 - See https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview
 - for conduit usage
 -
 - See http://hackage.haskell.org/package/cassava-0.1.0.1/docs/Data-Csv.html for
 - usage of Data.Csv package (cassava in cabal)
 -}

module Yahoo where

import Data.Conduit 
import Data.Csv
import Data.List (intercalate)
import Data.Vector hiding ((++), mapM_)
import Control.Applicative
import Control.Lens
import Control.Monad.Trans (liftIO, MonadIO)
import Network.Wreq (get, responseBody)
import qualified Data.Conduit.List as CL

-- Test code to see yahoo quote stream in ghci
source :: Source IO Int -- produces a stream of Ints
source = CL.sourceList [1..4]

sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

conduit :: Conduit YahooQuote IO String -- converts YahooQuotes into Strings
conduit = CL.map show

type Symbol = String

data YahooQuote = YahooQuote {
    symbol  :: Symbol,
    name    :: String,
    currency:: String,
    quote   :: Double,
    change  :: Double,
    volume  :: Int
} deriving (Show, Eq, Ord)

baseQuoteUri :: String
baseQuoteUri = "http://download.finance.yahoo.com/d/quotes.csv?f=snc4l1c6v&s="

-- | provide an instance of FromRecord to decode the csv we get back from yahoo
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

makeUrl :: [Symbol] -> String
makeUrl symbols = baseQuoteUri ++ (intercalate "+" symbols)

-- Make http request to yahoo for stock symbols and stream the result back
-- See 'Lifting Operations' in conduit documentation for orientation on this impl
getQuote :: MonadIO m => [Symbol] -> Source m YahooQuote
getQuote symbols = do
    r <- liftIO $ get (makeUrl symbols)
    case decode NoHeader (r ^. responseBody) of
        Left err -> liftIO $ putStrLn ("Failed to decode yahoo response: " ++ err)
        Right vals -> mapM_ yield (toList vals)

testSymbols = ["GOOG", "AAPL"]
testResult = (getQuote testSymbols) $$ conduit $= sink
