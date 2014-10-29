module Main where

{- Compile using 
 - ghc -o PortfolioCli Main.hs 
 -}

import Data.Conduit
import Data.List (intercalate)
import DbAdapter
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Yahoo

data Flag = Version | ShowPortfolio | Update | AddSymbol
    deriving (Show, Eq, Ord)

options :: [OptDescr Flag]
options = 
    [ Option ['v'] ["version"] (NoArg Version)         "Show version number"
    , Option ['s'] ["show"]    (NoArg ShowPortfolio)   "Show current portfolio"
    , Option ['u'] ["update"]  (NoArg Update)          "Show the market position"
    , Option ['a'] ["add"]     (NoArg AddSymbol)       "Add a symbol to the portfolio"
    ]

dispatch :: [(Flag, IO ())]
dispatch = 
    [ (Version, version)
    , (ShowPortfolio, showPortfolio)
    , (AddSymbol, addSymbol)
    , (Update, update)
    ]

main = do
    args <- getArgs
    progName <- getProgName
    let (flags, nonopts, msgs) = getOpt RequireOrder options args
    case flags of
        [mode]  -> case lookup mode dispatch of
                    (Just action) -> action
        _       -> ioError (userError (concat msgs ++ helpMessage))
            where
            helpMessage = usageInfo ("Usage: " ++ progName ++ " MODE") options

version :: IO ()
version = putStrLn "0.0.0.1"

prettyPrint :: [String] -> String
prettyPrint = intercalate "\t" 

showPortfolio :: IO ()
showPortfolio = do
    positions <- fetchPositions
    let prettyPositions = map (prettyPrint . toStrings) positions
    mapM_ putStrLn prettyPositions

addSymbol :: IO ()
addSymbol = do
    putStrLn "Enter Symbol: "
    symbol <- getLine
    putStrLn "Enter Position: "
    position <- getLine
    putStrLn "Enter Currency: "
    currency <- getLine
    putStrLn "Enter Strike: "
    strike <- getLine

    res <- insertPosition (Position symbol 
                                    currency 
                                    (read position :: Double) 
                                    (read strike :: Double))

    case res of
        1 -> putStrLn "Added position to db"
        _ -> putStrLn "Failed to add position to db"
        

update :: IO ()
update = do
    symbols <- fetchSymbols 
    pop <- populateQuotesTable symbols
    prtf <- fetchPortfolio
    let prettyPrtf = map (prettyPrint . toStrings) prtf
    mapM_ putStrLn prettyPrtf
