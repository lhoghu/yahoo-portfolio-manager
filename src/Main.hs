module Main where

{- Compile using 
 - ghc -o PortfolioCli Main.hs 
 -}

import Data.Conduit
import Data.List (intercalate)
import Data.Version
import Database.HDBC
import DbAdapter
import Paths_yahoo_portfolio_manager
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Yahoo

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

version :: IO ()
version = putStrLn $ showVersion Paths_yahoo_portfolio_manager.version

prettyPrint :: [String] -> String
prettyPrint = intercalate "\t" 

showPortfolio :: IConnection conn => conn -> IO ()
showPortfolio conn = do
    positions <- fetchPositions conn
    let prettyPositions = map (prettyPrint . toStrings) positions
    mapM_ putStrLn prettyPositions

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
        

update :: IConnection conn => conn -> IO ()
update conn = do
    symbols <- fetchSymbols conn 
    pop <- populateQuotesTable conn symbols
    fxs <- updateFx conn
    prtf <- fetchPortfolio conn
    putStrLn . prettyPrint $ headers
    let prettyPrtf = map (prettyPrint . toStrings) prtf
    mapM_ putStrLn prettyPrtf

updateFx :: IConnection conn => conn -> IO ()
updateFx conn = do
    fxs <- fetchFx conn
    insertFx conn (map setFx fxs)

setFx :: Fx -> Fx
setFx (Fx "GBP" "GBp" _) = Fx "GBP" "GBp" 0.01
setFx (Fx "GBp" "GBP" _) = Fx "GBp" "GBP" 100.0
setFx (Fx to from _) = if to == from then Fx to from 1.0 else Fx to from 1.0
