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

import Data.List (intercalate)
import Data.Version
import Database.HDBC
import DbAdapter
import Paths_yahoo_portfolio_manager
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Yahoo (validateSymbol)

-- Set up the flags the user can pass in at the command line
data Flag = Version | ShowPortfolio | Update | AddSymbol | Lookup
    deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = 
    [ Option ['v'] ["version"] (NoArg Main.Version)    "Show version number"
    , Option ['s'] ["show"]    (NoArg ShowPortfolio)   "Show current portfolio"
    , Option ['u'] ["update"]  (NoArg Update)          "Show the market position"
    , Option ['a'] ["add"]     (NoArg AddSymbol)       "Add a symbol to the portfolio"
    , Option ['l'] ["lookup"]  (NoArg Lookup)    "Lookup possible matches for a symbol in Yahoo"
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
            [Main.Version]  -> Main.version
            [ShowPortfolio] -> showPortfolio conn
            [AddSymbol]     -> addSymbol conn
            [Update]        -> update conn
            [Lookup]        -> fail "Not implemented yet"
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
    sym <- getLine
    putStrLn "Enter Position: "
    pos <- getLine
    putStrLn "Enter Currency: "
    cur <- getLine
    putStrLn "Enter Strike: "
    str <- getLine

    symbolOk <- validateSymbol sym
    if symbolOk then do
        res <- insertPosition conn (Position 
                                        sym 
                                        cur 
                                        (read pos :: Double) 
                                        (read str :: Double))
        case res of
            1 -> putStrLn "Added position to db"
            _ -> putStrLn "Failed to add position to db"
     else putStrLn "Symbol not recognised by Yahoo. It has not been added to the db. Try using -l flag to see possible alternatives"
        
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
    populateQuotesTable conn symbols
    updateFx conn
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
