module Token (
    ByteCode (..),
    Token (..),
    showToken,
    showTokens
) where

import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | ByteCode Grammer
data ByteCode
    = -- | Load value to the stack.
      LoadVal Int
    | -- | Push value from memory to the stack.
      ReadVar Text.Text
    | -- | Write value at the top of the stack to the enviroment.
      WriteVar Text.Text
    | -- | Return value at the top of the stack.
      ReturnVal
    | -- | Set LR = PC
      SavePC
    | -- | Pushes PC to the stack.
      ReadPC
    | -- | Pushes AC to the stack.
      ReadAC
    | -- | Pushes the value in LR to the stack.
      ReadLR
    | -- | Set PC to the value at the top of the stack.
      LoadPC
    | -- | Set AC to the value at the top of the stack.
      LoadAC
    | -- | Set LR to the value at the top of the stack.
      LoadLR
    | -- | if ac == o then pc = ac
      JE
    | -- | if ac < o then pc = ac
      JL
    | -- | if ac <= o then pc = ac
      JLE
    | -- | if ac > o then pc = ac
      JG
    | -- | if ac >= o then pc = ac
      JGE
    | Add
    | Sub
    | Mul
    | Div
    | -- | Print Stack Machine.
      DebugSM
    deriving (Show, Eq)

type Token = (Int, ByteCode)

showToken :: Token -> String
showToken (ln, t) 
  = mconcat [ "Line ", show ln, " -> ", show t ]

showTokens :: Vector.Vector Token -> String
showTokens = Vector.foldl (\acc tok -> acc ++ "\n\t" ++ showToken tok) mempty