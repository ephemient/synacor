{-# LANGUAGE TupleSections, ViewPatterns #-}

module Synacor.Disassembler (disassemble) where

import Control.Monad (void)
import Data.Bits ((.&.))
import qualified Data.Set as Set
import Data.Word (Word16)
import Foreign (Ptr, peekElemOff)
import Synacor.Instruction (Instruction(..), SValue(..), readInstruction)
import Synacor.SequentialReader (getPosition, lift, runSequentialReader, sequentially)
import Text.Printf (printf)

disassemble :: (Ptr Word16, Word16) -> IO ()
disassemble (mem, len) = do
    let reader = readInstruction $ sequentially $ peekElemOff mem . fromIntegral
        disas visited (Set.minView -> Just (loc@(pc, prev), queue)) = do
            (pc', instruction) <- runSequentialReader reader pc
            printf "%04x: %v\n" pc instruction
            let visited' = Set.insert loc visited
                ok loc@(pc, _) = pc < len && not (Set.member loc visited')
            disas visited' $ Set.union queue $ Set.fromList $ filter ok $ case instruction of
                Halt -> []
                Set (SReg reg) (SInt imm) -> [(pc', Just (reg, imm))]
                Jmp (SInt pc'') -> [(pc'', Nothing)]
                Jmp (SReg reg) | Just (reg', pc'') <- prev, reg == reg' -> [(pc'', Nothing)]
                Jmp (SReg _) -> error $ printf "unhandled computed jump: %04x: %v" pc instruction
                Jt (SInt imm) (SInt pc'') -> (, Nothing) <$> if imm == 0 then [pc'] else [pc'']
                Jt _ (SInt pc'') -> (, Nothing) <$> [pc', pc'']
                Jt (SReg reg) (SReg reg')
                  | Just (reg'', pc'') <- prev, reg == reg', reg == reg'' -> [(pc'', Nothing)]
                Jt _ (SReg _) -> error $ printf "unhandled computed jump: %04x: %v" pc instruction
                Jf (SInt imm) (SInt pc'') -> (, Nothing) <$> if imm == 0 then [pc''] else [pc']
                Jf _ (SInt pc'') -> (, Nothing) <$> [pc', pc'']
                Jf _ (SReg _) -> error $ printf "unhandled computed jump: %04x: %v" pc instruction
                Add (SReg reg) (SInt a) (SInt b)-> [(pc', Just (reg, (a + b) .&. 0x7fff))]
                Call (SInt pc'') -> (, Nothing) <$> [pc', pc'']
                Call (SReg reg)
                  | Just (reg', pc'') <- prev, reg == reg' -> (, Nothing) <$> [pc', pc'']
                Call (SReg _) -> error $ printf "unhandled computed jump: %04x: %v" pc instruction
                Ret -> []
                _ -> [(pc', Nothing)]
        disas _ _ = return ()
    disas Set.empty $ Set.singleton (0, Nothing)
