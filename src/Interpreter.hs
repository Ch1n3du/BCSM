module Interpreter (

) where

import qualified Data.Vector as Vector

import StackMachine
import Token


runToken :: Token -> CompRes -> CompRes
runToken (ln, tok) prev  = case prev of
  Left x -> Left x
  Right r -> case r of
    ValRes n   -> Right $ ValRes n
    DebugRes s -> Right $ DebugRes s
    SMRes sm   -> case tok of
        LoadVal v    -> loadVal v sm
        ReadVar l    -> readVar ln l sm
        WriteVar l v -> writeVar ln l v sm
        ReturnVal    -> returnVal ln sm
        SavePC       -> savePC sm
        ReadPC       -> readPC sm
        LoadPC       -> loadPC ln sm
        ReadAC       -> readAC sm
        LoadAC       -> loadAC ln  sm
        ReadLR       -> readLR sm
        LoadLR       -> loadLR ln sm
        JE           -> je sm
        JL           -> jl sm
        JLE          -> jle sm
        JG           -> jg sm
        JGE          -> jge sm
        Add          -> add ln sm
        Sub          -> sub ln sm
        Mul          -> mul ln sm
        Div          -> div_ ln sm
        DebugSM      -> debugSM sm

runTokens :: Vector.Vector Token -> CompRes
runTokens = foldr 