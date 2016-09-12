module Compiler.GCC.JIT.Monad.Context where

#include <libgccjit.h>

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign.Types
import Compiler.GCC.JIT.Foreign.Context

import Data.ByteString (ByteString)

import Control.Monad.IO.Class (liftIO)

import System.IO (Handle)

-- * Context functions
withContext :: JIT a -- ^ JIT code to load run in the context
            -> IO a
withContext j = do
    ctx <- contextAquire
    r <- evalJIT ctx j
    contextRelease ctx
    return r

withChildContext :: JIT a -- ^ JIT code to run in the child context
                 -> JIT a
withChildContext j = do
    ctx <- context
    cCtx <- liftIO $ contextNewChildContext ctx
    r <- liftIO $ evalJIT cCtx j
    release cCtx
    return r

dumpToFile :: ByteString -> Bool -> JIT ()
dumpToFile = inContext2 contextDumpToFile

setLogfile :: Maybe Handle -> Int -> Int -> JIT ()
setLogfile = inContext3 contextSetLogfile

dumpReproducerToFile :: ByteString -> JIT ()
dumpReproducerToFile = inContext1 contextDumpReproducerToFile

-- * Option functions
setStringOption :: JITStrOption -> Maybe ByteString -> JIT ()
setStringOption = inContext2 contextSetStrOption

setBoolOption :: JITBoolOption -> Bool -> JIT ()
setBoolOption = inContext2 contextSetBoolOption

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks
allowUnreachableBlocks :: Bool -> JIT ()
allowUnreachableBlocks = inContext1 contextAllowUnreachableBlocks
#endif

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver
useExternalDriver :: Bool -> JIT ()
useExternalDriver = inContext1 contextUseExternalDriver
#endif

setIntOption :: JITIntOption -> Int -> JIT ()
setIntOption = inContext2 contextSetIntOption

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
addCommandLineOption :: ByteString -> JIT ()
addCommandLineOption = inContext1 contextAddCommandLineOption
#endif
