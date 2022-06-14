module Interpreter (
    runCurrentToken,
    runToken',
    runToken,
    runTokensTillEnd',
    runTokensTillEnd,
) where

import Control.Lens hiding (element)
import qualified Data.Vector as Vector

import StackMachine
import Token

runCurrentToken :: StackMachine -> CompRes
runCurrentToken sm =
    case curr_in of
        Nothing -> Right $ NullExit sm
        Just (ln, bc) -> case bc of
            LoadVal v -> loadVal v sm
            ReadVar l -> readVar ln l sm
            WriteVar l -> writeVar ln l sm
            ReturnVal -> returnVal ln sm
            SavePC -> savePC sm
            ReadPC -> readPC sm
            ReadAC -> readAC sm
            ReadLR -> readLR sm
            LoadPC -> loadPC ln sm
            LoadAC -> loadAC ln sm
            LoadLR -> loadLR ln sm
            JE -> je sm
            JL -> jl sm
            JG -> jg sm
            JLE -> jle sm
            JGE -> jge sm
            Add -> add ln sm
            Sub -> sub ln sm
            Mul -> mul ln sm
            Div -> div_ ln sm
            DebugSM -> debugSM sm
  where
    ins = sm ^. smInstructions
    pc_ = sm ^. pc
    curr_in =
        if pc_ < length ins
            then Just $ ins Vector.! (pc_)
            else Nothing

runToken' :: CompRes -> CompRes
runToken' res = case res of
    Left l -> Left l
    Right r -> case r of
        SMRes sm -> runCurrentToken sm
        rest -> Right $ rest

runToken :: [Token] -> CompRes
runToken ts = runToken' $ Right $ SMRes $ smFromTokens ts

runTokensTillEnd' :: CompRes -> CompRes
runTokensTillEnd' res = case res of
    Left l -> Left l
    Right r -> case r of
        SMRes sm -> runTokensTillEnd' $ runCurrentToken sm
        rest -> Right rest

runTokensTillEnd :: [Token] -> CompRes
runTokensTillEnd ts = runTokensTillEnd' $ Right $ SMRes $ smFromTokens ts
