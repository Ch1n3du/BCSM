module CLI (
    runCLI,
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import StackMachine
import Interpreter
import Scanner

runCLI :: [Text.Text] -> IO ()
runCLI args = case length args of
    1 -> runFile (head args)
    _ -> putStrLn "Invalid input"

runFile :: Text.Text -> IO ()
runFile filepath = do

    raw <- TIO.readFile (Text.unpack filepath)

    let tokens = scanLines raw

    putStrLn "\n"
    case tokens of
        Left e -> print e
        Right ts -> print $ runTokensTillEnd ts
    putStrLn "\n"


-- Helper Functions
printCompRes :: CompRes -> IO ()
printCompRes cr = case cr of
    Left e  -> print e
    Right r -> print r 
