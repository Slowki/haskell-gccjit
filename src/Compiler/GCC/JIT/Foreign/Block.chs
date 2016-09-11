{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.GCC.JIT.Foreign.Block where

#include <libgccjit.h>
{# context lib = "libgccjit" #}

{#import Compiler.GCC.JIT.Foreign.Types#}
import Compiler.GCC.JIT.Foreign.Utilities

import Foreign.C.Types
import Foreign.Ptr (nullPtr)

import Data.ByteString (ByteString, useAsCString)
import Data.Maybe (isJust, fromJust)

-- * Block functions
-- | gcc_jit_function_new_block
functionNewBlock :: JITFunction -> Maybe ByteString -> IO JITBlock
functionNewBlock f n = if isJust n
                           then useAsCString (fromJust n) $ \s -> {#call unsafe gcc_jit_function_new_block#} f s
                           else {#call unsafe gcc_jit_function_new_block#} f (nullPtr)

-- | gcc_jit_block_add_eval
blockAddEval :: JITBlock -> Maybe JITLocation -> JITRValue -> IO ()
blockAddEval b l = {#call unsafe gcc_jit_block_add_eval#} b (fromMaybeLocation l)

-- | gcc_jit_block_add_assignment
blockAddAssignment :: JITBlock -> Maybe JITLocation -> JITLValue -> JITRValue -> IO ()
blockAddAssignment b l lv rv = {#call unsafe gcc_jit_block_add_assignment#} b (fromMaybeLocation l) lv rv

-- | gcc_jit_block_end_with_void_return
blockEndWithVoidReturn :: JITBlock -> Maybe JITLocation -> IO ()
blockEndWithVoidReturn b l = {#call unsafe gcc_jit_block_end_with_void_return#} b (fromMaybeLocation l)

-- | gcc_jit_block_end_with_return
blockEndWithReturn :: JITBlock -> Maybe JITLocation -> JITRValue -> IO ()
blockEndWithReturn b l = {#call unsafe gcc_jit_block_end_with_return#} b (fromMaybeLocation l)
