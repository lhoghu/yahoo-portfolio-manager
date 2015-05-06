{- | 
Command line interface
 
Can be called with any the following options:

* -v, --version. Display the current version

* -s, --show. Show the portfolio positions. This displays purely the information
entered by the user

* -u, --update. Update the latest quote data from yahoo and display it along 
with the associated user defined portfolio position data

* -a, --add. Add a new symbol to the portfolio. The user will be prompted for 
the symbol, number of units, purchase price and currency

* -d, --div. Add a new dividend payment. The user will be prompted for 
the symbol, dividend per unit and divident payment date

* -l, --lookup. Look for matches in Yahoo of a symbol string or substring. Yahoo returns up to 10 possible matches
-}

module Data.YahooPortfolioManager.Cli.Main (main) where

import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Data.List (intercalate)
import qualified Data.Version as DV
import Database.HDBC
import Data.YahooPortfolioManager.DateUtils (toDate)
import Data.YahooPortfolioManager.DbAdapter
import qualified Paths_yahoo_portfolio_manager as P
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Text.Regex
import Text.Tabular
import Text.Tabular.AsciiArt
import Data.YahooPortfolioManager.Types
import Data.YahooPortfolioManager.Yahoo ( validateSymbol
                                        , lookupSymbol
                                        , LookupSymbol (..))

-- Set up the flags the user can pass in at the command line
data Flag = Version | ShowPortfolio | Update | AddSymbol | 
            AddDividend | ShowDividends | Lookup | LoadHisto
    deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = 
    [ Option ['v'] ["version"] (NoArg Version)    "Show version number"
    , Option ['s'] ["show"]    (NoArg ShowPortfolio)   "Show current portfolio"
    , Option ['u'] ["update"]  (NoArg Update)          "Show the market position"
    , Option ['a'] ["add"]     (NoArg AddSymbol)       "Add a symbol to the portfolio"
    , Option ['d'] ["div"]     (NoArg AddDividend)     "Add a dividend payment"
    , Option ['p'] ["pay"]     (NoArg ShowDividends)   "Show all dividend payments"
    , Option ['h'] ["histo"]   (NoArg LoadHisto)       "Load historical quotes from yahoo"
    , Option ['l'] ["lookup"]  (NoArg Lookup)          "Lookup possible matches for a symbol in Yahoo"
    ]

main :: IO ()
main = handleSqlError $
    do
        args <- getArgs
        dbfp <- dbFile
        conn <- connect dbfp
        progName <- getProgName
        let (flags, _, msgs) = getOpt RequireOrder options args
        case flags of
            [Version]  -> version
            [ShowPortfolio] -> showPortfolio conn
            [AddSymbol]     -> addSymbol conn
            [AddDividend]   -> addDividend conn
            [ShowDividends] -> showDividends conn
            [Update]        -> update conn
            [LoadHisto]     -> loadHistoQuotes conn
            [Lookup]        -> Data.YahooPortfolioManager.Cli.Main.lookup
            _               -> ioError (userError (concat msgs ++ helpMessage))
                where
                helpMessage = usageInfo ("Usage: " ++ progName ++ " MODE") 
                                        options
        disconnect conn

--------------------------------------------------------------------------------
-- Version impl
--------------------------------------------------------------------------------
version :: IO ()
version = putStrLn $ DV.showVersion P.version

--------------------------------------------------------------------------------
-- ShowPortfolio impl
--------------------------------------------------------------------------------

{- Print the portfolio, as entered by the user, to the terminal window -}
showPortfolio :: IConnection conn => conn -> IO ()
showPortfolio conn = do
    positions <- fetchPositions conn
    let prettyPositions = stringify prettyPrint positions
    mapM_ putStrLn prettyPositions

--------------------------------------------------------------------------------
-- ShowDividends impl
--------------------------------------------------------------------------------

{- Print the dividends, as entered by the user, to the terminal window -}
showDividends :: IConnection conn => conn -> IO ()
showDividends conn = do
    dividends <- fetchDividends conn
    let prettyDividends = stringify prettyPrint dividends
    mapM_ putStrLn prettyDividends

--------------------------------------------------------------------------------
-- AddSymbol impl
--------------------------------------------------------------------------------
{-
Add a symbol to the db. The user is prompted for the yahoo symbol, 
the postition (number of units), the currency the bought in and the price
-}
addSymbol :: IConnection conn => conn -> IO ()
addSymbol conn = do
    putStrLn "Enter Symbol: "
    sym <- getLine
    putStrLn "Enter Position: "
    pos <- getLine
    putStrLn "Enter Currency: "
    cur <- getLine
    putStrLn "Enter Strike: "
    str <- getLine
    putStrLn "Enter Trade date (yyyy-mm-dd): "
    dte <- getLine

    symOk <- validateSymbol sym
    let curOk = validateCurrency cur
        posOk = validatePosition pos
        dteOk = validateDate dte
        strOk = validatePrice str in
        if not symOk then 
            putStrLn "Symbol not recognised by Yahoo. It has not been added to the db. Try using -l flag to see possible alternatives"
        else if not curOk then
            putStrLn "Currency not in recognised format. It should be a three character string. The symbol has not been added to the db"
        else if not posOk then
            putStrLn "Position not in recognised format. It should be a number. The symbol has not been added to the db"
        else if not strOk then
            putStrLn "Price not in recognised format. It should be a number. The symbol has not been added to the db"
        else if not dteOk then
            putStrLn "Trade date not in recognised format. It should be of form yyyy-mm-dd. The symbol has not been added to the db"
        else do
            res <- insertPosition conn (Position 
                                            sym 
                                            cur 
                                            dte
                                            (read pos :: Double) 
                                            (read str :: Double))
            case res of
                1 -> putStrLn "Added position to db"
                _ -> putStrLn "Failed to add position to db"

validateCurrency :: String -> Bool 
validateCurrency ccy = case matchRegex (mkRegex "^[A-Za-z]{3}$") ccy of 
                        Just _ -> True
                        Nothing -> False
    

validatePosition :: String -> Bool
validatePosition pos = case matchRegex (mkRegex "^[0-9]*\\.?[0-9]*$") pos of 
                        Just _ -> True
                        Nothing -> False

validatePrice :: String -> Bool
validatePrice price = case matchRegex (mkRegex "^[0-9]*\\.?[0-9]*$") price of 
                        Just _ -> True
                        Nothing -> False

validateDate :: String -> Bool 
validateDate dte = case matchRegex (mkRegex "^[0-9]{4}-[0-9]{2}-[0-9]{2}$") dte of 
                        Just _ -> True
                        Nothing -> False

--------------------------------------------------------------------------------
-- AddDividend impl
--------------------------------------------------------------------------------
{-
Add a dividend to the db. The user is prompted for the yahoo symbol, 
the dividend (per unit) and the dividend date.
-}
addDividend :: IConnection conn => conn -> IO ()
addDividend conn = do
    putStrLn "Enter Symbol: "
    sym <- getLine
    putStrLn "Enter Dividend: "
    div <- getLine
    putStrLn "Enter Dividend date (yyyy-mm-dd): "
    dte <- getLine

    symOk <- validateSymbol sym
    let dteOk = validateDate dte in
        if not symOk then 
            putStrLn "Symbol not recognised by Yahoo. The dividend has not been added to the db. Try using -l flag to see possible alternatives"
        else if not dteOk then
            putStrLn "Date not in recognised format (yyyy-mm-dd). The dividend has not been added to the db"
        else do
            res <- insertDividend conn (Dividend
                                            sym 
                                            (read div:: Double) 
                                            dte)
            case res of
                1 -> putStrLn "Added dividend to db"
                _ -> putStrLn "Failed to add dividend to db"

--------------------------------------------------------------------------------
-- Update impl
--------------------------------------------------------------------------------
{- Human readable table headers to prepend to the portfolio table ready
  for output at the command line
 -}
headers :: [String]
headers = ["Alloc", "Price", "Cost", "Current", "Chg", "(%)", "Divs", "PnL", "(%)"]

prettyPrint :: [String] -> String
prettyPrint = intercalate "\t" 

tabulate :: [Portfolio] -> String
tabulate ps = render id id id table
    where
        table = Table (Group NoLine (map (Header . prtfsymbol) ps)) 
                      (Group NoLine (map Header headers)) 
                      (map (drop 1 . toStrings) ps)

{- Print the portfolio, including the latest quotes from yahoo
to the terminal window
-}
update :: IConnection conn => conn -> IO ()
update conn = do
    symbols <- fetchSymbols conn 
    populateQuotesTable conn symbols
    updateFx conn
    prtf <- fetchPortfolio conn
    putStrLn $ tabulate prtf
    -- putStrLn . prettyPrint $ headers
    -- let prettyPrtf = stringify prettyPrint prtf
    -- mapM_ putStrLn prettyPrtf

--------------------------------------------------------------------------------
-- Lookup impl
--------------------------------------------------------------------------------
{-
Lookup a symbol in yahoo to see potential matches
-}
lookup :: IO ()
lookup = do
    putStrLn "Enter Symbol: "
    sym <- getLine
    matches <- lookupSymbol sym $$ CL.consume
    mapM_ putStrLn (matchStrings matches)
    where
        matchStrings :: [LookupSymbol] -> [String]
        matchStrings = map (\m -> prettyPrint [resSymbol m, 
                                               resTypeDisp m, 
                                               resExchDisp m, 
                                               resName m])

--------------------------------------------------------------------------------
-- Histo quote retrieval
--------------------------------------------------------------------------------
{- Load historical quotes from Yahoo into the db
 -}
loadHistoQuotes :: IConnection conn => conn -> IO ()
loadHistoQuotes conn = do
    putStrLn "Enter symbol: "
    symbol <- getLine
    putStrLn "Enter start date: "
    startDate <- getLine
    putStrLn "Enter end date: "
    endDate <- getLine

    symbolOk <- validateSymbol symbol
    let fromOk = validateDate startDate
        toOk = validateDate endDate in
        if not symbolOk then 
            putStrLn "Symbol not recognised by Yahoo. No history has been added to the db. Try using -l flag to see possible alternatives"
        else if not fromOk then
            putStrLn "Start date not in recognised format (yyyy-mm-dd). No history has been added to the db"
        else if not toOk then
            putStrLn "End date not in recognised format (yyyy-mm-dd). No history has been added to the db"
        else do
            populateHistoQuotes conn symbol (toDate startDate) (toDate endDate)
