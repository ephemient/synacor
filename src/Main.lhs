~~~ {.haskell}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import Control.Monad (foldM)
import Data.ByteString.Lazy (ByteString, readFile, toChunks)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word16)
import Foreign (Ptr, advancePtr, allocaArray, castPtr, copyArray, minusPtr)
import Paths_synacor (getDataFileName)
import Prelude hiding (readFile)
import Synacor.Disassembler (disassemble)
import Synacor.Machine (run)
import System.Console.CmdArgs.Implicit (Data, Typeable, (&=), auto, cmdArgs, def, explicit, help, modes, name, typFile)

data Command
  = Run {binPath :: Maybe FilePath}
  | Disassemble {binPath :: Maybe FilePath}
  deriving (Data, Typeable)

main :: IO ()
main = do
    args <- cmdArgs $ modes
      [ Run
          { binPath = def &= typFile &= explicit &= name "f" &= name "file" &= help "challenge.bin"
          } &= auto &= help "Run the machine image"
      , Disassemble
          { binPath = def &= typFile &= explicit &= name "f" &= name "file" &= help "challenge.bin"
          } &= help "Disassemble the machine image"
            &= name "dis" &= name "disas" &= name "disassemble"
      ]
    case args of
        Run {..} -> withBin binPath run
        Disassemble {..} -> withBin binPath disassemble

withBin :: Maybe FilePath -> ((Ptr Word16, Word16) -> IO a) -> IO a
withBin filePath f = do
    bin <- maybe (getDataFileName "challenge.bin") return filePath >>= readFile
    allocaArray 32768 $ \mem -> do
        end <- foldM copy (castPtr mem) $ toChunks bin
        f (mem, fromIntegral $ castPtr end `minusPtr` mem)
  where copy dst chunk = unsafeUseAsCStringLen chunk $ \(src, len) -> do
            copyArray dst src len
            return $ advancePtr dst len
~~~
