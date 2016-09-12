{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.GCC.JIT.Foreign.Utilities where

#include <stdio.h>

import Foreign.C.Types
import Foreign.Ptr

import Data.Maybe
import Data.ByteString (ByteString, useAsCString)
-- TODO find non compiler specific alternative
import GHC.IO.Handle (Handle, hDuplicate)

#ifdef _WIN32
    --TODO figure out how to do this on Windows
#else
import System.Posix.IO (handleToFd)
import System.Posix.Types (Fd(..))

type FilePointer = Ptr CFile

handleToFile :: Handle -> ByteString -> IO FilePointer
handleToFile h m = do
    (Fd fd) <- hDuplicate h >>= handleToFd -- Duplicated to prevent closing the input handle
    castPtr <$> useAsCString m ({#call unsafe fdopen#} fd)
#endif

-- | Convert enums to their CInt values
enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum

-- | Convert True to 1 and False to 0
boolToCInt :: Bool -> CInt
boolToCInt True  = 1
boolToCInt False = 0
