module Interpreter (

) where

import StackMachine
import ByteCode

runToken :: Token -> CompRes -> CompRes
runToken tok prev  = case prev of
  Left x -> Left x
  Right r -> case r of
    ValRes n   -> Right $ ValRes n
    DebugRes s -> Right $ DebugRes s
    SMRes sm   -> case sm of
        LoadVal v    -> loadVal v sm
        ReadVar l    ->
        WriteVar l v ->
        ReturnVal    ->
        ReadPC       ->
        LoadPC       -> 
        ReadAC       ->
        LoadAC       ->
        ReadLR       ->
        LoadLR       ->
        JE           ->
        JL           ->
        JLE          ->