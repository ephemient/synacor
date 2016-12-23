~~~ {.haskell}
module Main where

import Control.Monad (foldM_)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)
import Data.ByteString.Lazy (readFile, toChunks)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Char (chr, ord)
import Data.Word (Word16, Word8)
import Foreign (advancePtr, allocaArray, castPtr, copyArray, peekElemOff, pokeElemOff)
import Paths_synacor (getDataFileName)
import Prelude hiding (readFile)
import Synacor.Machine (Machine(..), MachineState(..), loopMachine)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    filename <- case args of arg:_ -> return arg; _ -> getDataFileName "challenge.bin"
    allocaArray 32768 $ \mem -> do
        let copy dst chunk = unsafeUseAsCStringLen chunk $ \(src, len) -> do
                copyArray dst src len
                return $ advancePtr dst len
        toChunks <$> readFile filename >>= foldM_ copy (castPtr mem)
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
~~~
