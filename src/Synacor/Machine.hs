{-# LANGUAGE RecordWildCards #-}

module Synacor.Machine (Machine(..), MachineState(..), loopMachine, run, stepMachine) where

import Control.Monad (liftM2)
import Control.Monad.Fail (MonadFail)
import Data.Char (chr, ord)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)
import Foreign (Ptr, peekElemOff, pokeElemOff)
import Data.Bits ((.&.), (.|.), complement)
import Data.Ix (inRange, index)
import Data.Word (Word16, Word8)
import Synacor.Instruction (Instruction(..), SValue(..), readInstruction)
import Synacor.SequentialReader (runSequentialReader, sequentially)

data (Monad m) => Machine m = Machine
  { readMemory :: Word16 -> m Word16
  , writeMemory :: Word16 -> Word16 -> m ()
  , readRegister :: Word8 -> m Word16
  , writeRegister :: Word8 -> Word16 -> m ()
  , readInput :: m Word16
  , writeOutput :: Word16 -> m ()
  }

data MachineState = MachineState
  { stack :: [Word16]
  , pc :: Word16
  } deriving (Eq, Ord, Read, Show)

load :: (Monad m) => Machine m -> SValue -> m Word16
load _ (SInt imm) = return imm
load m (SReg reg) = readRegister m reg

store :: (Monad m) => Machine m -> SValue -> Word16 -> m ()
store m (SReg reg) value = writeRegister m reg $ value .&. 0x7fff

stepMachine :: (MonadFail m) => Machine m -> MachineState -> m (Maybe MachineState)
stepMachine machine@Machine {..} state@MachineState {..} = do
    (pc', instruction) <- runSequentialReader (readInstruction $ sequentially readMemory) pc
    case instruction of
        Halt -> return Nothing
        Set dst src -> load machine src >>= store machine dst >> return (Just state {pc = pc'})
        Push src -> do
            val <- load machine src
            return $ Just state {stack = val:stack, pc = pc'}
        Pop dst | val:stack' <- stack ->
            store machine dst val >> return (Just state {stack = stack', pc = pc'})
        Eq dst a b -> do
            equ <- liftM2 (==) (load machine a) (load machine b)
            store machine dst $ if equ then 1 else 0
            return $ Just state {pc = pc'}
        Gt dst a b -> do
            gts <- liftM2 (>) (load machine a) (load machine b)
            store machine dst $ if gts then 1 else 0
            return $ Just state {pc = pc'}
        Jmp pc -> do pc' <- load machine pc; return $ Just state {pc = pc'}
        Jt src pc -> do
            val <- load machine src
            pc'' <- load machine pc
            return $ Just state {pc = if val /= 0 then pc'' else pc'}
        Jf src pc -> do
            val <- load machine src
            pc'' <- load machine pc
            return $ Just state {pc = if val == 0 then pc'' else pc'}
        Add dst a b -> do
            liftM2 (+) (load machine a) (load machine b) >>= store machine dst
            return $ Just state {pc = pc'}
        Mult dst a b -> do
            liftM2 (*) (load machine a) (load machine b) >>= store machine dst
            return $ Just state {pc = pc'}
        Mod dst a b -> do
            liftM2 (mod) (load machine a) (load machine b) >>= store machine dst
            return $ Just state {pc = pc'}
        And dst a b -> do
            liftM2 (.&.) (load machine a) (load machine b) >>= store machine dst
            return $ Just state {pc = pc'}
        Or dst a b -> do
            liftM2 (.|.) (load machine a) (load machine b) >>= store machine dst
            return $ Just state {pc = pc'}
        Not dst src ->
            load machine src >>= store machine dst . complement >> return (Just state {pc = pc'})
        RMem dst src -> do
            load machine src >>= readMemory >>= store machine dst
            return $ Just state {pc = pc'}
        WMem dst src -> do
            dst' <- load machine dst
            load machine src >>= writeMemory dst'
            return $ Just state {pc = pc'}
        Call pc'' -> do
            pc''' <- load machine pc''
            return $ Just state {stack = pc':stack, pc = pc'''}
        Ret | pc':stack' <- stack -> return $ Just state {stack = stack', pc = pc'}
        Out src -> load machine src >>= writeOutput >> return (Just state {pc = pc'})
        In dst -> readInput >>= store machine dst >> return (Just state {pc = pc'})
        Noop -> return $ Just state {pc = pc'}
        _ -> fail $ "illegal state at " ++ show pc ++ show (instruction, state)

loopMachine :: (MonadFail m) => Machine m -> MachineState -> m ()
loopMachine machine state = stepMachine machine state >>= maybe (return ()) (loopMachine machine)

run :: (Ptr Word16, Word16) -> IO ()
run (mem, _) = do
    regs <- newArray (0, 7) 0 :: IO (IOUArray Word8 Word16)
    let machine = Machine
          { readMemory = peekElemOff mem . fromIntegral
          , writeMemory = pokeElemOff mem . fromIntegral
          , readRegister = readArray regs
          , writeRegister = writeArray regs
          , readInput = fromIntegral . ord <$> getChar
          , writeOutput = putChar . chr . fromIntegral
          }
    loopMachine machine MachineState {stack = [], pc = 0}
