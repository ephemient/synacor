module Synacor.Disassembler (disassemble) where

import Control.Monad (void)
import Data.Word (Word16)
import Foreign (Ptr, peekElemOff)
import Synacor.Instruction (readInstruction)
import Synacor.SequentialReader (getPosition, lift, runSequentialReader, sequentially)
import Text.Printf (printf)

disassemble :: (Ptr Word16, Int) -> IO ()
disassemble (mem, len) = do
    let reader = readInstruction $ sequentially $ peekElemOff mem
        disas = do
            pc <- getPosition
            if pc < len then reader >>= lift . printf "%04x: %v\n" pc >> disas else return ()
    void $ runSequentialReader disas 0
