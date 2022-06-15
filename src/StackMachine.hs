{-# LANGUAGE TemplateHaskell #-}

module StackMachine (
    StackMachine (..),
    smStack,
    smInstructions,
    pc,
    ac,
    lr,
    smEnviroment,
    smFromTokens,
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
    debugSM,
) where

import Control.Lens hiding (element)
import qualified Control.Lens.Getter as Getter
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Stack
import Token

type Enviroment = Map.Map Text.Text Int

data StackMachine = StackMachine
    { _pc :: Int
    , _ac :: Int
    , _lr :: Int
    , _smEnviroment :: Enviroment
    , _smStack :: Stack Int
    , _smInstructions :: Vector.Vector Token
    }

makeLenses ''StackMachine

instance Show StackMachine where
    show s = mconcat [ "\nStack: ", show (s ^. smStack), "\n"
                     , "Program Counter: ", show (s ^. pc), "\n"
                     , "Accumulator: ", show (s ^. ac), "\n"
                     , "Loop Register: ", show (s ^. lr), "\n"
                     , "Enviroment: ", show (s ^. smEnviroment), "\n"
                     , "Instructions: ", showTokens (s ^. smInstructions)
                     ]


smFromTokens :: [Token] -> StackMachine
smFromTokens ins =
    StackMachine
        { _smStack = []
        , _smInstructions = Vector.fromList ins
        , _pc = 0                                -- ^ Program Counter
        , _ac = 0                                -- ^ Accumulator
        , _lr = 0                                -- ^ Loop Register
        , _smEnviroment = Map.empty
        }

data CompErr
    = VarNone Int Text.Text
    | ShortStack Int
    | Undefined Int

instance Show CompErr where
    show (VarNone ln identifier) = mconcat ["No such variable as ", Text.unpack identifier, " on line ", show ln, "."]
    show (ShortStack ln)         = mconcat ["Stack too short for operation on line ", show ln, "."]
    show (Undefined ln)          = mconcat ["Undefined error on line ", show ln, "."]

data CompOk
    = SMRes StackMachine
    | ValRes Int
    | DebugRes Int Text.Text
    | NullExit StackMachine

ul :: String
ul = "\n_________________"

instance Show CompOk where
    show (SMRes sm)      = mconcat ["\nNONEXITED_SM:\n", ul, show sm]
    show (ValRes v)      = mconcat ["\nRETURNED_VALUE: ", show v]
    show (DebugRes ln s) = mconcat ["\n\nDEBUG_SNAPSHOT ", "\nLINE: ", show ln, "\n", Text.unpack s]
    show (NullExit sm)   = mconcat ["\nEXITED_SM:\n", ul, show sm]

type CompRes = Either CompErr CompOk

incrementSmPc :: StackMachine -> StackMachine
incrementSmPc = pc %~ (+ 1)

pushSmStack :: Int -> StackMachine -> StackMachine
pushSmStack x sm = smStack %~ (\s -> push x s) $ sm

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
writeVar :: Int -> Text.Text -> StackMachine -> CompRes
writeVar ln identifier sm =
    case pop (sm ^. smStack) of
        Nothing -> Left $ VarNone ln identifier
        Just (_st, val) -> Right $ SMRes $ incrementSmPc $ onSmEnviroment (Map.insert identifier val) sm
  where
    onSmEnviroment f sm_ = smEnviroment %~ f $ sm_

-- | Returns value at the top of the stack
returnVal :: Int -> StackMachine -> CompRes
returnVal ln sm =
    case pop (sm ^. smStack) of
        Nothing -> Left $ ShortStack ln
        Just (_, x) -> Right $ ValRes x

type SmRegisterGetter = Getter.Getting Int StackMachine Int

-- | Saves PC to LR
savePC :: StackMachine -> CompRes
savePC sm = Right $ SMRes $ incrementSmPc $ lr .~ (sm ^. pc) $ sm

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
        Nothing -> Left $ ShortStack ln
        Just (xs, x) -> Right $ SMRes $ incrementSmPc $ regSetter .~ x $ smStack .~ xs $ sm

loadPC :: Int -> StackMachine -> CompRes
loadPC = loadSmReg pc

loadAC :: Int -> StackMachine -> CompRes
loadAC = loadSmReg ac

loadLR :: Int -> StackMachine -> CompRes
loadLR = loadSmReg lr

jump :: (Int -> Int -> Bool) -> StackMachine -> CompRes
jump f sm = Right $ SMRes $ incrementSmPc $ pc .~ newPc $ sm
  where
    newPc =
        if f (sm ^. ac) 0
            then sm ^. lr
            else sm ^. pc

je :: StackMachine -> CompRes
je = jump (==)

jl :: StackMachine -> CompRes
jl = jump (<)

jle :: StackMachine -> CompRes
jle = jump (<=)

jg :: StackMachine -> CompRes
jg = jump (>)

jge :: StackMachine -> CompRes
jge = jump (>=)

binStackOp :: (Int -> Int -> Int) -> Int -> StackMachine -> CompRes
binStackOp f ln sm = case popApply f (sm ^. smStack) of
    Nothing -> Left $ ShortStack ln
    Just xs -> Right $ SMRes $ incrementSmPc $ smStack .~ xs $ sm

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

debugSM :: Int -> StackMachine -> CompRes
debugSM ln sm = Right $ DebugRes ln $ Text.pack $ show sm

-- LOAD_VAL 1
-- ADD

-- READ_AC
-- LOAD_VAL 1
-- SUB
-- LOAD_AC

-- JG
-- DEBUG_SM