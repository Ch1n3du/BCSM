module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified System.Environment as Environment

import CLI
import Interpreter

-- Main Function
main :: IO ()
main = do
    args_ <- Environment.getArgs
    let args = map Text.pack args_
    runCLI args
