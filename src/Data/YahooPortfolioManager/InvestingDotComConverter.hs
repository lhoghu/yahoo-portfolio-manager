{-| Module to take historical data copied from e.g. 
    http://www.investing.com/currencies/eur-gbp-historical-data
    and stored in a text file, and convert it to haskell structures 
    for further processing
 -}
module InvestingDotComConverter where

import Data.Char (ord)
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as DV
import Control.Applicative
import Data.Time

data HistoricalFx = HistoricalFx {
    date    :: String,
    last    :: Double,
    open    :: Double,
    high    :: Double,
    low     :: Double, 
    change  :: String
} deriving (Show)

instance FromRecord HistoricalFx where
    parseRecord v
        | DV.length v == 6 = HistoricalFx <$>
                                    v .! 0 <*>
                                    v .! 1 <*>
                                    v .! 2 <*>
                                    v .! 3 <*>
                                    v .! 4 <*>
                                    v .! 5
        | otherwise     = fail "Unable to parse data as HistoricalFx"

decodeOptions = defaultDecodeOptions {
    decDelimiter = fromIntegral $ ord '\t'
}

decodeTabDelimited :: FromRecord a => BS.ByteString -> Maybe [a]
decodeTabDelimited s = case Data.Csv.decodeWith decodeOptions HasHeader s of
    Left _          -> Nothing
    Right decoded   -> Just $ DV.toList decoded

decodeHistoricalFx :: FilePath -> IO (Maybe [HistoricalFx])
-- decodeHistoricalFx f = do
--     encoded <- BS.readFile f
--     return $ parseDates <$> decodeTabDelimited encoded
decodeHistoricalFx f = BS.readFile f >>= process
    where process encoded = return $ parseDates <$> decodeTabDelimited encoded

parseDates :: [HistoricalFx] -> [HistoricalFx]
parseDates = map updateDates
    where updateDates fx = fx { date = doParse $ date fx }
          doParse = showGregorian . parseTimeOrError True defaultTimeLocale "%b %d, %Y"
