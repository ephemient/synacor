{-# LANGUAGE GADTs #-}

module Synacor.Instruction (Instruction(..), SValue(..), readInstruction) where

import Control.Monad.Fail (MonadFail(fail))
import Data.Word (Word16, Word8)
import Prelude hiding (fail)
import Synacor.SequentialReader (SequentialReader)

data SValue = SInt Word16 | SReg Word8 deriving (Eq, Ord, Show)
data Instruction where
    Halt :: Instruction
    Set :: SValue -> SValue -> Instruction
    Push :: SValue -> Instruction
    Pop :: SValue -> Instruction
    Equ :: SValue -> SValue -> SValue -> Instruction
    Gts :: SValue -> SValue -> SValue -> Instruction
    Jmp :: SValue -> Instruction
    Jt :: SValue -> SValue -> Instruction
    Jf :: SValue -> SValue -> Instruction
    Add :: SValue -> SValue -> SValue -> Instruction
    Mult :: SValue -> SValue -> SValue -> Instruction
    Mod :: SValue -> SValue -> SValue -> Instruction
    And :: SValue -> SValue -> SValue -> Instruction
    Or :: SValue -> SValue -> SValue -> Instruction
    Not :: SValue -> SValue -> Instruction
    RMem :: SValue -> SValue -> Instruction
    WMem :: SValue -> SValue -> Instruction
    Call :: SValue -> Instruction
    Ret :: Instruction
    Out :: SValue -> Instruction
    In :: SValue -> Instruction
    Noop :: Instruction
  deriving (Eq, Ord, Show)

readInstruction :: (MonadFail m, Enum k, Show k) =>
    SequentialReader m k Word16 -> SequentialReader m k Instruction
readInstruction reader = do
    let reader' = do
            value <- reader
            if 0 <= value && value < 32768
            then return $ SInt value
            else if 32768 <= value && value < 32776
            then return $ SReg $ fromIntegral $ value - 32768
            else fail $ "value out of bounds: " ++ show value
    opcode <- reader
    maybe (fail $ "unknown opcode: " ++ show opcode) id $ lookup opcode
      [ (0, return Halt)
      , (1, return Set <*> reader' <*> reader')
      , (2, return Push <*> reader')
      , (3, return Pop <*> reader')
      , (4, return Equ <*> reader' <*> reader' <*> reader')
      , (5, return Gts <*> reader' <*> reader' <*> reader')
      , (6, return Jmp <*> reader')
      , (7, return Jt <*> reader' <*> reader')
      , (8, return Jf <*> reader' <*> reader')
      , (9, return Add <*> reader' <*> reader' <*> reader')
      , (10, return Mult <*> reader' <*> reader' <*> reader')
      , (11, return Mod <*> reader' <*> reader' <*> reader')
      , (12, return And <*> reader' <*> reader' <*> reader')
      , (13, return Or <*> reader' <*> reader' <*> reader')
      , (14, return Not <*> reader' <*> reader')
      , (15, return RMem <*> reader' <*> reader')
      , (16, return WMem <*> reader' <*> reader')
      , (17, return Call <*> reader')
      , (18, return Ret)
      , (19, return Out <*> reader')
      , (20, return In <*> reader')
      , (21, return Noop)
      ]
