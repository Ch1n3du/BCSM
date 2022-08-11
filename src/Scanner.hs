module Scanner (
    ScanRes,
    scanLine,
    scanLines,
    cleanLines,
) where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Token

data ScanErr
    = UnexpectedNumberOfTokens Int
    | ExpectedNumber Int Text.Text
    | UnknownToken Int Text.Text
    deriving (Show)

type ScanRes = Either ScanErr [Token]
type ScanRes_ = Either ScanErr Token

scanLine :: (Int, Text.Text) -> ScanRes_
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
                "DEBUG_SM" -> wrap DebugSM
                _ -> Left $ UnknownToken ln arg_1
        2 ->
            case arg_1 of
                "WRITE_VAR" -> wrap $ WriteVar arg_2
                "READ_VAR" -> wrap $ ReadVar arg_2
                "LOAD_VAL" ->
                    if isNumber arg_2
                        then wrap $ LoadVal (read arg_2_ :: Int)
                        else Left $ ExpectedNumber ln arg_2
                _ -> Left $ UnknownToken ln arg_1
        _ -> Left $ UnexpectedNumberOfTokens ln
  where
    pieces = Text.words lexeme
    arg_1 = pieces !! 0
    arg_2 = pieces !! 1
    arg_2_ = Text.unpack arg_2

    wrap :: ByteCode -> ScanRes_
    wrap t = Right $ (ln, t)
    isNumber = Text.all Char.isDigit

cleanLines :: Text.Text -> [(Int, Text.Text)]
cleanLines = filterNulls . stripComments . numberLines 
  where
    numberLines t  = zip [1 ..] $ map Text.strip $ (Text.lines t)
    stripComments = map (\(ln, l) -> (ln, head $ Text.splitOn "//" l))
    filterNulls    = filter (\(ln, l) -> not $ Text.null l)

scanLines :: Text.Text -> ScanRes
scanLines t = foldl parse (Right []) raw
  where
    raw :: [ScanRes_]
    raw = (map scanLine) $ cleanLines t

    parse (Right acc) (Right r2) = Right $ acc ++ [r2]
    parse (Left e)    _ = Left e
    parse _           (Left e) = Left e
