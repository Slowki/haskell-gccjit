{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.GCC.JIT.Foreign.Expression where

#include <libgccjit.h>
{# context lib = "libgccjit" #}

{#import Compiler.GCC.JIT.Foreign.Types#}
import Compiler.GCC.JIT.Foreign.Utilities

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.ByteString (ByteString, useAsCString)
import Data.Maybe

-- * Expression functions

-- | gcc_jit_context_new_binary_op
contextNewBinaryOp :: JITContext -> Maybe JITLocation -> JITBinaryOp -> JITType -> JITRValue -> JITRValue -> IO JITRValue
contextNewBinaryOp c l t = {#call unsafe gcc_jit_context_new_binary_op#} c (fromMaybeLocation l) (enumToCInt t)

-- | gcc_jit_context_new_unary_op
contextNewUnaryOp :: JITContext -> Maybe JITLocation -> JITUnaryOp -> JITType -> JITRValue -> IO JITRValue
contextNewUnaryOp c l t = {#call unsafe gcc_jit_context_new_unary_op#} c (fromMaybeLocation l) (enumToCInt t)

-- | gcc_jit_context_new_comparison
contextNewComparison :: JITContext -> Maybe JITLocation -> JITComparison -> JITRValue -> JITRValue -> IO JITRValue
contextNewComparison c l t = {#call unsafe gcc_jit_context_new_comparison#} c (fromMaybeLocation l) (enumToCInt t)

-- | gcc_jit_context_new_call
contextNewCall :: JITContext -> Maybe JITLocation -> JITFunction -> [JITRValue] -> IO JITRValue
contextNewCall c l f as = withArray (map extractPointer as) $ \ar -> {#call unsafe gcc_jit_context_new_call#} c (fromMaybeLocation l) f (fromIntegral $ length as) (castPtr ar :: Ptr JITRValue)
    where
        extractPointer (JITRValue p) = p --JITRValue isn't Storable, but the pointer it encapsulates is

-- | gcc_jit_context_new_call_through_ptr
contextNewCallThroughPtr :: JITContext -> Maybe JITLocation -> JITRValue -> [JITRValue] -> IO JITRValue
contextNewCallThroughPtr c l fp as = withArray (map extractPointer as) $ \ar -> {#call unsafe gcc_jit_context_new_call_through_ptr#} c (fromMaybeLocation l) fp (fromIntegral $ length as) (castPtr ar :: Ptr JITRValue)
    where
        extractPointer (JITRValue p) = p --JITRValue isn't Storable, but the pointer it encapsulates is

-- | gcc_jit_context_new_cast
contextNewCast :: JITContext -> Maybe JITLocation -> JITRValue -> JITType -> IO JITRValue
contextNewCast c l = {#call unsafe gcc_jit_context_new_cast#} c (fromMaybeLocation l)

-- | gcc_jit_context_new_array_access
contextNewArrayAccess :: JITContext -> Maybe JITLocation -> JITRValue -> JITRValue -> IO JITLValue
contextNewArrayAccess r l = {#call unsafe gcc_jit_context_new_array_access#} r $ fromMaybeLocation l

-- * 'JITRValue' functions

#ifdef LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call
-- | gcc_jit_rvalue_set_bool_require_tail_call
rvalueSetBoolRequireTailCall :: JITRValue -> Bool -> IO ()
rvalueSetBoolRequireTailCall r v = {#call unsafe gcc_jit_rvalue_set_bool_require_tail_call#} r (boolToCInt v)
#endif

-- | gcc_jit_rvalue_get_type
rvalueGetType :: JITRValue -> IO JITType
rvalueGetType = {#call unsafe gcc_jit_rvalue_get_type#}

-- | gcc_jit_rvalue_dereference
rvalueDereference :: JITRValue -> Maybe JITLocation -> IO JITLValue
rvalueDereference r l = {#call unsafe gcc_jit_rvalue_dereference#} r $ fromMaybeLocation l

-- | gcc_jit_rvalue_access_field
rvalueAccessField :: JITRValue -> Maybe JITLocation -> JITField -> IO JITRValue
rvalueAccessField r l = {#call unsafe gcc_jit_rvalue_access_field#} r $ fromMaybeLocation l

-- | gcc_jit_rvalue_dereference_field
rvalueDereferenceField :: JITRValue -> Maybe JITLocation -> JITField -> IO JITLValue
rvalueDereferenceField r l = {#call unsafe gcc_jit_rvalue_dereference_field#} r $ fromMaybeLocation l

-- * 'JITLValue' functions

-- | gcc_jit_lvalue_get_address
lvalueGetAddress :: JITLValue -> Maybe JITLocation -> IO JITRValue
lvalueGetAddress v l = {#call unsafe gcc_jit_lvalue_get_address#} v $ fromMaybeLocation l

-- | gcc_jit_lvalue_access_field
lvalueAccessField :: JITLValue -> Maybe JITLocation -> JITField -> IO JITLValue
lvalueAccessField v l = {#call unsafe gcc_jit_lvalue_access_field#} v $ fromMaybeLocation l

-- The created C bindings are inserted at the end of the file
-- * C functions
