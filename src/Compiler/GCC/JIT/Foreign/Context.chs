{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.GCC.JIT.Foreign.Context where

#include <libgccjit.h>
{# context lib = "libgccjit" #}

{#import Compiler.GCC.JIT.Foreign.Types#}
import Compiler.GCC.JIT.Foreign.Utilities

import Data.ByteString (ByteString, useAsCString)
import Foreign.Ptr

-- * Context functions

-- | gcc_jit_context_acquire
contextAquire :: IO JITContext
contextAquire = {#call unsafe gcc_jit_context_acquire#}

-- | gcc_jit_context_new_child_context
newChildJITContext :: JITContext -> IO JITContext
newChildJITContext = {#call unsafe gcc_jit_context_new_child_context#}

-- | gcc_jit_context_release, free the given 'JITContext' and all associated memory
contextRelease :: JITContext -> IO ()
contextRelease = {#call unsafe gcc_jit_context_release#}

-- * Settings functions

-- | gcc_jit_context_set_int_option
contextSetIntOption :: JITContext -> JITIntOption -> Int -> IO ()
contextSetIntOption c e v = {#call unsafe gcc_jit_context_set_int_option#} c (enumToCInt e) (fromIntegral v)

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

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
-- | gcc_jit_context_add_command_line_option
contextAddCommandLineOption :: JITContext -> ByteString -> IO ()
contextAddCommandLineOption c b = useAsCString b $ \s -> {#call unsafe gcc_jit_context_add_command_line_option#} c s
#endif

-- * Result functions

-- | gcc_jit_context_compile
contextCompile :: JITContext -> IO JITResult
contextCompile = {#call unsafe gcc_jit_context_compile#}

-- | gcc_jit_result_get_code
resultGetCode :: JITResult -> ByteString -> IO (FunPtr a)
resultGetCode r n = castPtrToFunPtr <$> (useAsCString n $ {#call unsafe gcc_jit_result_get_code#} r)

-- | gcc_jit_result_release, free the given 'JITResult'
resultRelease :: JITResult -> IO ()
resultRelease = {#call unsafe gcc_jit_result_release#}

-- The created C bindings are inserted at the end of the file
-- * C functions
