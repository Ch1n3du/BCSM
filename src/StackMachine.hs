{-# LANGUAGE TemplateHaskell #-}

module StackMachine (
    StackMachine (..),
    CompErr (..),
    CompOk (..),
    CompRes (..),
    loadVal,
    readVar,
    writeVar,
    returnVal,
    savePC,
    readPC,
    loadPC,
    readAC,
    loadAC,
    readLR,
    loadLR,
    je,
    jl,
    jle,
    jg,
    jge,
    add,
    sub,
    mul,
    div_,
    debugSM
) where

import Control.Lens hiding (element)
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector
import qualified Control.Lens.Getter as Getter 

import Stack
import Token

type Enviroment = Map.Map Text.Text Int

data StackMachine = StackMachine
    { _smStack :: Stack Int
    , _smInstructions :: Vector.Vector Token
    , _pc :: Int
    , _ac :: Int
    , _lr :: Int
    , _smEnviroment :: Enviroment
    }
    deriving (Show)

makeLenses ''StackMachine

smFromTokens :: Vector.Vector Token -> StackMachine
smFromTokens ins =  
    StackMachine
    { _smStack        = []
    , _smInstructions = ins
    , _pc             = 0
    , _ac             = 0
    , _lr             = 0
    , _smEnviroment   = Map.empty
    }


data CompErr
    = VarNone Int Text.Text
    | ShortStack Int
    | Undefined Int

data CompOk
    = SMRes StackMachine
    | ValRes Int
    | DebugRes Text.Text

type CompRes = Either CompErr CompOk


incrementSmPc :: StackMachine -> StackMachine
incrementSmPc = pc %~ (+ 1)

pushSmStack :: Int -> StackMachine -> StackMachine
pushSmStack x sm =  smStack %~ (\s -> push x s) $ sm


-- | Loads an value onto the stack.
loadVal :: Int -> StackMachine -> CompRes
loadVal x sm = Right $ SMRes $ incrementSmPc $ pushSmStack x sm

-- | Read value in memory
readVar :: Int -> Text.Text -> StackMachine -> CompRes
readVar ln identifier sm =
    case Map.lookup identifier (sm ^. smEnviroment) of
        Nothing -> Left $ VarNone ln identifier
        Just x -> Right $ SMRes $ incrementSmPc $ pushSmStack x sm

-- | Writes the top of the stack to the enviroment using the identifier.
writeVar :: Int -> Text.Text -> Int -> StackMachine -> CompRes
writeVar ln identifier val sm
    = case pop (sm ^. smStack) of
        Nothing       -> Left $ VarNone ln identifier
        Just (_st, a) -> Right $ SMRes $ incrementSmPc $ onSmEnviroment (Map.insert identifier val) sm
  where 
      onSmEnviroment f sm_ = smEnviroment %~ f $ sm_ 

-- | Returns value at the top of the stack
returnVal :: Int -> StackMachine -> CompRes
returnVal ln sm =
    case pop (sm ^. smStack) of
        Nothing     -> Left $ ShortStack ln
        Just (_, x) -> Right $ ValRes x
    
type SmRegisterGetter = Getter.Getting Int StackMachine Int 

savePC :: StackMachine -> CompRes
savePC sm = Right $ SMRes $ lr .~ (sm ^. pc) $ sm

readSmReg :: SmRegisterGetter -> StackMachine -> CompRes
readSmReg regGetter sm = Right $ SMRes $ incrementSmPc $ newSm
  where
    newStack = push (sm ^. regGetter) (sm ^. smStack) 
    newSm = smStack .~ newStack $ sm

readPC :: StackMachine -> CompRes
readPC = readSmReg pc

readAC :: StackMachine -> CompRes
readAC = readSmReg ac

readLR :: StackMachine -> CompRes
readLR = readSmReg lr

type SmRegisterSetter = ASetter StackMachine StackMachine Int Int

loadSmReg :: SmRegisterSetter -> Int -> StackMachine -> CompRes
loadSmReg regSetter ln sm = 
    case pop (sm ^. smStack) of
        Nothing      -> Left $ ShortStack ln
        Just (xs, x) -> Right $ SMRes $ incrementSmPc $ regSetter .~ x $ smStack .~ xs $ sm

loadPC :: Int -> StackMachine -> CompRes
loadPC = loadSmReg pc

loadAC :: Int -> StackMachine -> CompRes
loadAC = loadSmReg ac

loadLR :: Int -> StackMachine -> CompRes
loadLR = loadSmReg lr

jump :: (Int -> Int -> Bool) -> StackMachine -> CompRes
jump f sm = Right $ SMRes $ pc .~ newPc $ sm 
  where 
    newPc = if f (sm ^. ac) 0 
            then sm ^. lr
            else sm ^. pc

je  :: StackMachine -> CompRes
je = jump (==)

jl  :: StackMachine -> CompRes
jl = jump (<)

jle :: StackMachine -> CompRes
jle = jump (<=)

jg  :: StackMachine -> CompRes
jg = jump (>)

jge :: StackMachine -> CompRes
jge = jump (>=)

binStackOp :: (Int -> Int -> Int) -> Int -> StackMachine -> CompRes
binStackOp f ln sm = case popApply f (sm ^. smStack) of
    Nothing  -> Left $ ShortStack ln
    Just xs -> Right $ SMRes $ smStack .~ xs $ sm 

-- | Executes Add Command
add :: Int -> StackMachine -> CompRes
add = binStackOp (+)

-- | Executes Sub Command
sub :: Int -> StackMachine -> CompRes
sub = binStackOp (-)

-- | Executes Mul Command
mul :: Int -> StackMachine -> CompRes
mul = binStackOp (*)

-- | Executes Div Command
div_ :: Int -> StackMachine -> CompRes
div_ = binStackOp (div)

debugSM :: StackMachine -> CompRes
debugSM sm = Right $ DebugRes $ Text.pack $ show sm