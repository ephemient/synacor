{-# LANGUAGE GADTs, NamedFieldPuns, ViewPatterns #-}

module Synacor.Instruction (Instruction(..), SValue(..), readInstruction) where

import Control.Monad.Fail (MonadFail(..))
import Data.Char (chr, isControl, isHexDigit, isPrint, isSpace, ord)
import Data.Word (Word16, Word8)
import Numeric (showHex)
import Prelude hiding (fail)
import Synacor.SequentialReader (SequentialReader)
import Text.ParserCombinators.ReadP (between, char, choice, munch1, string)
import Text.ParserCombinators.ReadPrec (lift, readPrec_to_P)
import Text.Printf (FieldFormat(..), PrintfArg(..), errorBadFormat, formatString, vFmt)
import Text.Read (Read(readListPrec, readPrec))

data SValue = SInt Word16 | SReg Word8 deriving (Eq, Ord)
data Instruction where
    Halt :: Instruction
    Set :: SValue -> SValue -> Instruction
    Push :: SValue -> Instruction
    Pop :: SValue -> Instruction
    Eq :: SValue -> SValue -> SValue -> Instruction
    Gt :: SValue -> SValue -> SValue -> Instruction
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
  deriving (Eq, Ord)

instance Show SValue where
    showsPrec _ (SInt imm) = showString "$" . showHex imm
    showsPrec _ (SReg reg) = showChar 'R' . shows reg

instance PrintfArg SValue where
    formatArg x (vFmt 's' -> fmt@FieldFormat {fmtChar = 's'}) =
        formatString (show x) fmt {fmtChar = 's'}
    formatArg _ FieldFormat {fmtChar} = errorBadFormat fmtChar

instance Read SValue where
    readPrec = lift $ choice
      [ char '$' >> SInt . read <$> munch1 isHexDigit
      , char 'R' >> SReg <$> readPrec_to_P readPrec 0
      , SInt . fromIntegral . ord <$> readPrec_to_P readPrec 0
      ]

instance Show Instruction where
    showsPrec _ Halt = showString "halt"
    showsPrec _ (Set dst src) = showString "set " . shows dst . showChar ' ' . shows src
    showsPrec _ (Push src) = showString "push " . shows src
    showsPrec _ (Pop dst) = showString "pop " . shows dst
    showsPrec _ (Eq dst a b) =
        showString "eq " . shows dst . showChar ' ' . shows a . showChar ' ' . shows b
    showsPrec _ (Gt dst a b) =
        showString "gt " . shows dst . showChar ' ' . shows a . showChar ' ' . shows b
    showsPrec _ (Jmp pc) = showString "jmp *" . shows pc
    showsPrec _ (Jt src pc) = showString "jt " . shows src . showString " *" . shows src
    showsPrec _ (Jf src pc) = showString "jf " . shows src . showString " *" . shows src
    showsPrec _ (Add dst a b) =
        showString "add " . shows dst . showChar ' ' . shows a . showChar ' ' . shows b
    showsPrec _ (Mult dst a b) =
        showString "mult " . shows dst . showChar ' ' . shows a . showChar ' ' . shows b
    showsPrec _ (Mod dst a b) =
        showString "mod " . shows dst . showChar ' ' . shows a . showChar ' ' . shows b
    showsPrec _ (And dst a b) =
        showString "and " . shows dst . showChar ' ' . shows a . showChar ' ' . shows b
    showsPrec _ (Or dst a b) =
        showString "or " . shows dst . showChar ' ' . shows a . showChar ' ' . shows b
    showsPrec _ (Not dst src) = showString "not " . shows dst . showChar ' ' . shows src
    showsPrec _ (RMem dst src) =
        showString "rmem " . shows dst . showString " (" . shows src . showChar ')'
    showsPrec _ (WMem dst src) = showString "wmem (" . shows dst . showString ") " . shows src
    showsPrec _ (Call pc) = showString "call *" . shows pc
    showsPrec _ Ret = showString "ret"
    showsPrec _ (Out (SInt (chr . fromIntegral -> c))) | isControl c || isPrint c =
        showString "out " . shows c
    showsPrec _ (Out src) = showString "out " . shows src
    showsPrec _ (In dst) = showString "in " . shows dst
    showsPrec _ Noop = showString "noop"

instance PrintfArg Instruction where
    formatArg x (vFmt 's' -> fmt@FieldFormat {fmtChar = 's'}) =
        formatString (show x) fmt {fmtChar = 's'}
    formatArg _ FieldFormat {fmtChar} = errorBadFormat fmtChar

instance Read Instruction where
    readPrec = lift $ choice
      [ string "halt" *> return Halt
      , string "set" *> return Set <*> sp <*> sp
      , string "push" *> return Push <*> sp
      , string "pop" *> return Pop <*> sp
      , string "eq" *> return Eq <*> sp <*> sp <*> sp
      , string "gt" *> return Gt <*> sp <*> sp <*> sp
      , string "jmp" *> return Jmp <*> as
      , string "jt" *> return Jt <*> sp <*> as
      , string "jf" *> return Jf <*> sp <*> as
      , string "add" *> return Add <*> sp <*> sp <*> sp
      , string "mult" *> return Mult <*> sp <*> sp <*> sp
      , string "mod" *> return Mod <*> sp <*> sp <*> sp
      , string "and" *> return And <*> sp <*> sp <*> sp
      , string "or" *> return Or <*> sp <*> sp <*> sp
      , string "not" *> return Not <*> sp <*> sp
      , string "rmem" *> return RMem <*> sp <*> par
      , string "wmem" *> return WMem <*> par <*> sp
      , string "call" *> return Call <*> as
      , string "ret" *> return Ret
      , string "out" *> return Out <*> sp
      , string "in" *> return In <*> sp
      , string "noop" *> return Noop
      ] where
        sp = munch1 isSpace *> readPrec_to_P readPrec 0
        as = munch1 isSpace *> char '*' *> readPrec_to_P readPrec 0
        par = munch1 isSpace *> char '(' *> readPrec_to_P readPrec 0 <* char ')'

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
    maybe (fail $ "unknown opcode: " ++ showHex opcode "") id $ lookup opcode
      [ (0, return Halt)
      , (1, return Set <*> reader' <*> reader')
      , (2, return Push <*> reader')
      , (3, return Pop <*> reader')
      , (4, return Eq <*> reader' <*> reader' <*> reader')
      , (5, return Gt <*> reader' <*> reader' <*> reader')
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
