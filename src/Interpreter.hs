module Interpreter (

) where

import qualified Data.Vector as Vector
import Control.Lens hiding (element)

import StackMachine
import Token


runToken :: StackMachine -> CompRes
runToken sm = 
  case pc_ >= Vector.length ins of
    True -> Right $ NullExit
    False -> case bc of
        LoadVal v    -> loadVal v sm
        ReadVar l    -> readVar ln l sm
        WriteVar l v -> writeVar ln l v sm
        ReturnVal    -> returnVal ln sm
        SavePC       -> savePC sm
        ReadPC       -> readPC sm
        ReadAC       -> readAC sm
        ReadLR       -> readLR sm
        LoadPC       -> loadPC ln sm
        LoadAC       -> loadAC ln  sm
        LoadLR       -> loadLR ln sm
        JE           -> je sm
        JL           -> jl sm
        JG           -> jg sm
        JLE          -> jle sm
        JGE          -> jge sm
        Add          -> add ln sm
        Sub          -> sub ln sm
        Mul          -> mul ln sm
        Div          -> div_ ln sm
        DebugSM      -> debugSM sm
  where
    ins = sm ^. smInstructions
    pc_ = sm ^. pc
    (ln, bc) = ins Vector.! pc_