module Data.YahooPortfolioManager.Types where

-- | Provide a Symbol type for clarity. 
-- These should be the stock symbols used by Yahoo
type Symbol = String

{- | Haskell structure that represents the command line output table
  including yahoo quotes and portfolio position info
 -}
data Portfolio = Portfolio {
    prtfalloc       :: Double,
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

{-| Haskell structure that contains the portfolio position information
  entered by the user
 -}
data Position = Position {
    possymbol      :: String,
    poscurrency    :: String,
    posdate        :: String,
    posposition    :: Double,
    posstrike      :: Double
} deriving (Show, Ord, Eq)

{-| Haskell structure that contains the fx to convert yahoo quotes into
 the symbol currency entered by the user
 -}
data Fx = Fx {
    fxToCcy :: String,
    fxFromCcy :: String,
    fx :: Double,
    prtfFx :: Double
} deriving (Show, Ord, Eq)

{-| Haskell structure that contains the portfolio position information
  entered by the user
 -}
data Dividend = Dividend {
    divsymbol   :: String,
    dividend    :: Double,
    divdate     :: String
} deriving (Show, Ord, Eq)

