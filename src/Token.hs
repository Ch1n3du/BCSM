module Token (
    ByteCode (..),
    Token (..),
) where

import qualified Data.Text as Text

-- | ByteCode Grammer
data ByteCode
    = -- | Load value to the stack.
      LoadVal Int
    | -- | Push value from memory to the stack.
      ReadVar Text.Text
    | -- | Write value at the top of the stack to the enviroment.
      WriteVar Text.Text Int
    | -- | Return value at the top of the stack.
      ReturnVal
    | -- | Pushes PC to the stack.
      ReadPC
    | -- | Set PC to the value at the top of the stack.
      LoadPC
    | -- | Pushes AC to the stack.
      ReadAC
    | -- | Set AC to the value at the top of the stack.
      LoadAC
    | -- | Pushes value in LR to the stack.
      ReadLR
    | -- | Set PC to the value at the top of the stack.
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
