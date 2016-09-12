{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Compiler.GCC.JIT.Foreign.Context where

#include <libgccjit.h>
{# context lib = "libgccjit" #}

{#import Compiler.GCC.JIT.Foreign.Types#}
import Compiler.GCC.JIT.Foreign.Utilities

import Foreign.Ptr
import Foreign.C.Types

import Data.ByteString (ByteString, useAsCString, empty)
import Data.Maybe (fromMaybe)

import System.IO (Handle)

-- * Context functions

-- | gcc_jit_context_acquire
contextAquire :: IO JITContext
contextAquire = {#call unsafe gcc_jit_context_acquire#}

-- | gcc_jit_context_new_child_context
contextNewChildContext :: JITContext -> IO JITContext
contextNewChildContext = {#call unsafe gcc_jit_context_new_child_context#}

-- | gcc_jit_context_release, free the given 'JITContext' and all associated memory
contextRelease :: JITContext -> IO ()
contextRelease = {#call unsafe gcc_jit_context_release#}

-- * Debugging functions
-- | gcc_jit_context_dump_to_file
contextDumpToFile :: JITContext -> ByteString -> Bool -> IO ()
contextDumpToFile c fn ul = useAsCString fn $ \cs -> {#call unsafe gcc_jit_context_dump_to_file#} c cs (boolToCInt ul)

-- | gcc_jit_context_set_logfile
contextSetLogfile :: JITContext -> Maybe Handle -> Int -> Int -> IO ()
contextSetLogfile c (Just h) flgs v = handleToFile h "w" >>= \fp -> {#call unsafe gcc_jit_context_set_logfile#} c (castPtr fp) (fromIntegral flgs) (fromIntegral v)
contextSetLogfile c Nothing flgs v  = {#call unsafe gcc_jit_context_set_logfile#} c (castPtr nullPtr) (fromIntegral flgs) (fromIntegral v)

-- | gcc_jit_context_dump_reproducer_to_file
contextDumpReproducerToFile :: JITContext -> ByteString -> IO ()
contextDumpReproducerToFile c fn = useAsCString fn $ {#call unsafe gcc_jit_context_dump_reproducer_to_file#} c

-- TODO gcc_jit_context_enable_dump

-- * Option functions
-- | gcc_jit_context_set_str_option
contextSetStrOption :: JITContext -> JITStrOption -> Maybe ByteString -> IO ()
contextSetStrOption c e (Just v) = useAsCString v $ {#call unsafe gcc_jit_context_set_str_option#} c (enumToCInt e)
contextSetStrOption c e Nothing = {#call unsafe gcc_jit_context_set_str_option#} c (enumToCInt e) (castPtr nullPtr :: Ptr CChar)

-- | gcc_jit_context_set_bool_option
contextSetBoolOption :: JITContext -> JITBoolOption -> Bool -> IO ()
contextSetBoolOption c e v = {#call unsafe gcc_jit_context_set_bool_option#} c (enumToCInt e) (boolToCInt v)

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks
-- | gcc_jit_context_set_bool_allow_unreachable_blocks
contextAllowUnreachableBlocks :: JITContext -> Bool -> IO ()
contextAllowUnreachableBlocks c v = {#call unsafe gcc_jit_context_set_bool_allow_unreachable_blocks#} c $ boolToCInt v
#endif

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver
-- | gcc_jit_context_set_bool_use_external_driver
contextUseExternalDriver :: JITContext -> Bool -> IO ()
contextUseExternalDriver c v = {#call unsafe gcc_jit_context_set_bool_use_external_driver#} c $ boolToCInt v
#endif

-- | gcc_jit_context_set_int_option
contextSetIntOption :: JITContext -> JITIntOption -> Int -> IO ()
contextSetIntOption c e v = {#call unsafe gcc_jit_context_set_int_option#} c (enumToCInt e) (fromIntegral v)

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
-- | gcc_jit_context_add_command_line_option
contextAddCommandLineOption :: JITContext -> ByteString -> IO ()
contextAddCommandLineOption c b = useAsCString b $ {#call unsafe gcc_jit_context_add_command_line_option#} c
#endif

-- * Result functions

-- | gcc_jit_context_compile
contextCompile :: JITContext -> IO JITResult
contextCompile = {#call unsafe gcc_jit_context_compile#}

-- | gcc_jit_result_get_code
resultGetCode :: JITResult -> ByteString -> IO (FunPtr a)
resultGetCode r n = castPtrToFunPtr <$> (useAsCString n $ {#call unsafe gcc_jit_result_get_code#} r)

-- | gcc_jit_result_get_global
resultGetGlobal :: JITResult -> ByteString -> IO (Ptr a)
resultGetGlobal r n = castPtr <$> (useAsCString n $ {#call unsafe gcc_jit_result_get_global#} r)

-- | gcc_jit_result_release, free the given 'JITResult'
resultRelease :: JITResult -> IO ()
resultRelease = {#call unsafe gcc_jit_result_release#}

-- | gcc_jit_context_compile_to_file
contextCompileToFile :: JITContext -> JITOutputKind -> ByteString -> IO ()
contextCompileToFile c k fn = useAsCString fn $ {#call unsafe gcc_jit_context_compile_to_file#} c (enumToCInt k)

-- The created C bindings are inserted at the end of the file
-- * C functions
