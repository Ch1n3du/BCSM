module CLI (
    runCLI,
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import Interpreter
import Scanner

runCLI :: [Text.Text] -> IO ()
runCLI args = case length args of
    0 -> runREPL
    1 -> runFile (head args)
    _ -> putStrLn "Invalid input"

runFile :: Text.Text -> IO ()
runFile filepath = do

    raw <- TIO.readFile (Text.unpack filepath)
    -- putStrLn $ Text.unpack raw
    printHeader "LINES"
    print $ cleanLines raw

    let tokens = scanLines raw

    printHeader "\nTOKENS"
    print tokens

    printHeader "\nRESULT"
    case tokens of
        Left e -> print e
        Right ts -> print $ runTokensTillEnd ts
    putStrLn "\n"


-- Helper Functions
printHeader :: String -> IO ()
printHeader header = do 
    putStrLn $ "\n" ++ header ++ ":"
    putStrLn $ "________________\n"
