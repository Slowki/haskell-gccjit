{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.GCC.JIT.Foreign.Definition where

#include <libgccjit.h>
{# context lib = "libgccjit" #}

{#import Compiler.GCC.JIT.Foreign.Types#}
import Compiler.GCC.JIT.Foreign.Utilities

import Foreign.Ptr
import Foreign.Marshal.Array

import Data.ByteString (ByteString, useAsCString)

-- * Global functions

-- | gcc_jit_context_new_global
contextNewGlobal :: JITContext -> Maybe JITLocation -> JITGlobalKind -> JITType -> ByteString -> IO JITLValue
contextNewGlobal c l k t n = useAsCString n $ \cs -> {#call unsafe gcc_jit_context_new_global#} c (fromMaybeLocation l) (enumToCInt k) t cs

-- * Function functions

-- | gcc_jit_context_new_function
contextNewFunction :: JITContext -> Maybe JITLocation -> JITFunctionKind -> JITType -> ByteString -> [JITParam] -> Bool -> IO JITFunction
contextNewFunction c l k rt n ps isvar = withArray (map extractPointer ps) $
    \ar -> useAsCString n $
        \cs -> {#call unsafe gcc_jit_context_new_function#} c (fromMaybeLocation l) (enumToCInt k) rt cs (fromIntegral $ length ps) (castPtr ar :: Ptr JITParam) (boolToCInt isvar)
    where
        extractPointer (JITParam p) = p --JITParam isn't Storable, but the pointer it encapsulates is

-- | gcc_jit_context_new_param
contextNewParam :: JITContext -> Maybe JITLocation -> JITType -> ByteString -> IO JITParam
contextNewParam c l t n = useAsCString n $ \cs -> {#call unsafe gcc_jit_context_new_param#} c (fromMaybeLocation l) t cs

-- * Local functions
-- | gcc_jit_function_new_local
functionNewLocal :: JITFunction -> Maybe JITLocation -> JITType -> ByteString -> IO JITLValue
functionNewLocal f l t n = useAsCString n $ \cs -> {#call unsafe gcc_jit_function_new_local#} f (fromMaybeLocation l) t cs
