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

-}

module Main (main) where

import Data.Conduit
import Data.List (intercalate)
import Data.Version
import Database.HDBC
import DbAdapter
import Paths_yahoo_portfolio_manager
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Yahoo

-- Set up the flags the user can pass in at the command line
data Flag = Version | ShowPortfolio | Update | AddSymbol
    deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = 
    [ Option ['v'] ["version"] (NoArg Main.Version)    "Show version number"
    , Option ['s'] ["show"]    (NoArg ShowPortfolio)   "Show current portfolio"
    , Option ['u'] ["update"]  (NoArg Update)          "Show the market position"
    , Option ['a'] ["add"]     (NoArg AddSymbol)       "Add a symbol to the portfolio"
    ]

main = handleSqlError $
    do
        args <- getArgs
        dbfp <- dbFile
        conn <- connect dbfp
        progName <- getProgName
        let (flags, nonopts, msgs) = getOpt RequireOrder options args
        case flags of
            [Main.Version]  -> Main.version
            [ShowPortfolio] -> showPortfolio conn
            [AddSymbol]     -> addSymbol conn
            [Update]        -> update conn
            _               -> ioError (userError (concat msgs ++ helpMessage))
                where
                helpMessage = usageInfo ("Usage: " ++ progName ++ " MODE") 
                                        options
        disconnect conn

-- Version impl
version :: IO ()
version = putStrLn $ showVersion Paths_yahoo_portfolio_manager.version

-- ShowPortfolio impl

{- Print the portfolio, as entered by the user, to the terminal window -}
showPortfolio :: IConnection conn => conn -> IO ()
showPortfolio conn = do
    positions <- fetchPositions conn
    let prettyPositions = stringify prettyPrint positions
    mapM_ putStrLn prettyPositions

-- AddSymbol impl
{-
Add a symbol to the db. The user is prompted for the yahoo symbol, 
the postition (number of units), the currency the bought in and the price
-}
addSymbol :: IConnection conn => conn -> IO ()
addSymbol conn = do
    putStrLn "Enter Symbol: "
    symbol <- getLine
    putStrLn "Enter Position: "
    position <- getLine
    putStrLn "Enter Currency: "
    currency <- getLine
    putStrLn "Enter Strike: "
    strike <- getLine

    res <- insertPosition conn (Position 
                                    symbol 
                                    currency 
                                    (read position :: Double) 
                                    (read strike :: Double))

    case res of
        1 -> putStrLn "Added position to db"
        _ -> putStrLn "Failed to add position to db"
        
-- Update impl
{- Human readable table headers to prepend to the portfolio table ready
  for output at the command line
 -}
headers :: [String]
headers = ["Sym", "Ccy", "Pos", "@", "Quote", "Chg", "(%)", "Vol", "FX"]

prettyPrint :: [String] -> String
prettyPrint = intercalate "\t" 

{- Print the portfolio, including the latest quotes from yahoo
to the terminal window
-}
update :: IConnection conn => conn -> IO ()
update conn = do
    symbols <- fetchSymbols conn 
    pop <- populateQuotesTable conn symbols
    fxs <- updateFx conn
    prtf <- fetchPortfolio conn
    putStrLn . prettyPrint $ headers
    let prettyPrtf = stringify prettyPrint prtf
    mapM_ putStrLn prettyPrtf

updateFx :: IConnection conn => conn -> IO ()
updateFx conn = do
    fxs <- fetchFx conn
    insertFx conn (map setFx fxs)

-- Placeholder for fx functionality, pending retrieval from yahoo
setFx :: Fx -> Fx
setFx (Fx "GBP" "GBp" _) = Fx "GBP" "GBp" 0.01
setFx (Fx "GBp" "GBP" _) = Fx "GBp" "GBP" 100.0
setFx (Fx to from _) = if to == from then Fx to from 1.0 else Fx to from 1.0
