module Scanner where

import qualified Data.Text as Text
import qualified Data.Char as Char

import Token

data ScannerErr 
    = UnexpectedNumberOfTokens Int
    | ExpectedNumber Int Text.Text
    | UnknownToken Int Text.Text

type ScannerRes = Either ScannerErr Token

scanLine :: (Int, Text.Text) -> ScannerRes
scanLine (ln, lexeme) =
    case length pieces of
      1 -> 
        case arg_1 of
          "JE"         -> wrap JE
          "JL"         -> wrap JL
          "JLE"        -> wrap JLE
          "JG"         -> wrap JG
          "JGE"        -> wrap JGE
          "ADD"        -> wrap Add
          "SUB"        -> wrap Sub
          "MULTIPLY"   -> wrap Mul
          "DIVIDE"     -> wrap Div
          "READ_AC"    -> wrap ReadAC
          "READ_LR"    -> wrap ReadLR
          "LOAD_PC"    -> wrap LoadPC
          "LOAD_AC"    -> wrap LoadAC
          "LOAD_LR"    -> wrap LoadLR
          "RETURN_VAL" -> wrap ReturnVal
          "SAVE_PC"    -> wrap SavePC
          "READ_PC"    -> wrap ReadPC
          _            -> Left $ UnknownToken ln arg_1
      2 -> 
        case arg_1 of 
          "WRITE_VAR" -> wrap $ WriteVar arg_2
          "READ_VAR"  -> wrap $ ReadVar arg_2
          "LOAD_VAL"  -> if isNumber arg_2
                         then wrap $ LoadVal (read arg_2_ :: Int )
                         else Left $ ExpectedNumber ln arg_2
      _ -> Left $ UnexpectedNumberOfTokens ln
  where
    pieces = Text.words lexeme
    arg_1 = pieces !! 0
    arg_2 = pieces !! 1
    arg_2_ = Text.unpack arg_2 

    wrap t = Right $ (ln, t)

isNumber :: Text.Text -> Bool
isNumber = Text.any Char.isDigit