module Scanner (
    scanLine,
    scanLines
) where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Token

data ScanErr
    = UnexpectedNumberOfTokens Int
    | ExpectedNumber Int Text.Text
    | UnknownToken Int Text.Text
    deriving Show

type ScanRes = Either ScanErr Token

scanLine :: (Int, Text.Text) -> ScanRes
scanLine (ln, lexeme) =
    case length pieces of
        1 ->
            case arg_1 of
                "JE" -> wrap JE
                "JL" -> wrap JL
                "JLE" -> wrap JLE
                "JG" -> wrap JG
                "JGE" -> wrap JGE
                "ADD" -> wrap Add
                "SUB" -> wrap Sub
                "MULTIPLY" -> wrap Mul
                "DIVIDE" -> wrap Div
                "READ_AC" -> wrap ReadAC
                "READ_LR" -> wrap ReadLR
                "LOAD_PC" -> wrap LoadPC
                "LOAD_AC" -> wrap LoadAC
                "LOAD_LR" -> wrap LoadLR
                "RETURN_VAL" -> wrap ReturnVal
                "SAVE_PC" -> wrap SavePC
                "READ_PC" -> wrap ReadPC
                _ -> Left $ UnknownToken ln arg_1
        2 ->
            case arg_1 of
                "WRITE_VAR" -> wrap $ WriteVar arg_2
                "READ_VAR" -> wrap $ ReadVar arg_2
                "LOAD_VAL" ->
                    if isNumber arg_2
                        then wrap $ LoadVal (read arg_2_ :: Int)
                        else Left $ ExpectedNumber ln arg_2
        _ -> Left $ UnexpectedNumberOfTokens ln
  where
    pieces = Text.words lexeme
    arg_1 = pieces !! 0
    arg_2 = pieces !! 1
    arg_2_ = Text.unpack arg_2

    wrap t = Right $ (ln, t)
    isNumber = Text.all Char.isDigit

numberLines :: Text.Text -> [(Int, Text.Text)]
numberLines t = zip [1..] $ Text.lines t

processNullLine :: Vector.Vector (Int, Text.Text) -> (Int, Text.Text) -> Vector.Vector (Int, Text.Text)
processNullLine acc (ln, l) = 
        if Text.null strippedL
        then acc
        else acc <> Vector.singleton (ln, strippedL)
    where strippedL = Text.strip l
    
nonEmptyLines :: Text.Text -> Vector.Vector (Int, Text.Text)
nonEmptyLines t = foldl processNullLine Vector.empty $ numberLines t

scanLines :: Text.Text -> Vector.Vector ScanRes -> Either ScanErr (Vector.Vector Token)
scanLines t = Vector.foldl parse (Right Vector.empty)  --raw 
  where
    raw = Vector.map scanLine $ nonEmptyLines t

    parse (Right acc) (Right r2) = Right $ acc <> Vector.singleton r2
    parse (Left e)    _          = Left e
    parse  _          (Left e)   = Left e
