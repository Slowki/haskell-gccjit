module Compiler.GCC.JIT.Monad.Context where

#include <libgccjit.h>

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign.Types
import Compiler.GCC.JIT.Foreign.Context

import Data.ByteString (ByteString)

import Control.Monad.State
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
    state <- get
    cCtx <- liftIO $ contextNewChildContext ctx
    r <- liftIO $ evalJITState cCtx  state j
    release cCtx
    return r

withContextAndState :: s
                    -> JITState s a -- ^ JIT code to load run in the context
                    -> IO a
withContextAndState s j = do
    ctx <- contextAquire
    r <- evalJITState ctx s j
    contextRelease ctx
    return r

-- * Error-handling

getFirstError :: JITState s (Maybe ByteString)
getFirstError = inContext contextGetFirstError

getLastError :: JITState s (Maybe ByteString)
getLastError = inContext contextGetLastError

-- * Debugging functions
dumpToFile :: ByteString -> Bool -> JITState s ()
dumpToFile = inContext2 contextDumpToFile

setLogfile :: Maybe Handle -> Int -> Int -> JITState s ()
setLogfile = inContext3 contextSetLogfile

dumpReproducerToFile :: ByteString -> JITState s ()
dumpReproducerToFile = inContext1 contextDumpReproducerToFile

-- * Option functions
setStringOption :: JITStrOption -> Maybe ByteString -> JITState s ()
setStringOption = inContext2 contextSetStrOption

setBoolOption :: JITBoolOption -> Bool -> JITState s ()
setBoolOption = inContext2 contextSetBoolOption

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks
allowUnreachableBlocks :: Bool -> JITState s ()
allowUnreachableBlocks = inContext1 contextAllowUnreachableBlocks
#endif

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver
useExternalDriver :: Bool -> JITState s ()
useExternalDriver = inContext1 contextUseExternalDriver
#endif

setIntOption :: JITIntOption -> Int -> JITState s ()
setIntOption = inContext2 contextSetIntOption

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
addCommandLineOption :: ByteString -> JITState s ()
addCommandLineOption = inContext1 contextAddCommandLineOption
#endif
