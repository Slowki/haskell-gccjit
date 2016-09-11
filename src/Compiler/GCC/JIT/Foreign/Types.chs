{-|
Module      : Compiler.GCC.JIT.Foreign.Types
Description : libgccjit Types, enums, constructors, and casts
Stability   : experimental
Portability : POSIX

This module contains libgccjit types, enums, constructor functions, and cast functions
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.GCC.JIT.Foreign.Types where

#include <libgccjit.h>
{# context lib = "libgccjit" #}

import Compiler.GCC.JIT.Foreign.Utilities

import Foreign.Ptr
import Foreign.Marshal.Array

import Data.ByteString (ByteString, useAsCString)
import Data.ByteString.Unsafe (unsafePackCString)
import Data.Maybe (fromMaybe, isJust, fromJust)

-- * Type Enums
-- | gcc_jit_types
{# enum gcc_jit_types         as JITTypeName     {underscoreToCase} with prefix = "GCC_JIT_TYPE_" add prefix = "JIT" deriving (Eq, Show) #}

-- * Kind enums
-- | gcc_jit_output_kind
{# enum gcc_jit_output_kind   as JITOutputKind   {underscoreToCase} with prefix = "GCC_JIT_OUTPUT_KIND" add prefix = "JIT_OUTPUT_" deriving (Eq, Show) #}
-- | gcc_jit_function_kind
{# enum gcc_jit_function_kind as JITFunctionKind {underscoreToCase} with prefix = "GCC_" deriving (Eq, Show) #}
-- | gcc_jit_global_kind
{# enum gcc_jit_global_kind   as JITGlobalKind   {underscoreToCase} with prefix = "GCC_" deriving (Eq, Show) #}

-- * Option name enums
-- | gcc_jit_int_option
{# enum gcc_jit_str_option    as JITStrOption    {underscoreToCase} with prefix = "GCC_JIT_STR_OPTION" add prefix = "JIT" deriving (Eq, Show) #}
-- | gcc_jit_int_option
{# enum gcc_jit_int_option    as JITIntOption    {underscoreToCase} with prefix = "GCC_JIT_INT_OPTION" add prefix = "JIT" deriving (Eq, Show) #}
-- | gcc_jit_bool_option
{# enum gcc_jit_bool_option   as JITBoolOption   {underscoreToCase} with prefix = "GCC_JIT_BOOL_OPTION" add prefix = "JIT" deriving (Eq, Show) #}

-- * Operation type enums
-- | gcc_jit_binary_op
{# enum gcc_jit_binary_op     as JITBinaryOp     {underscoreToCase} with prefix = "GCC_JIT_BINARY_" add prefix = "JIT" deriving (Eq, Show) #}
-- | gcc_jit_unary_op
{# enum gcc_jit_unary_op      as JITUnaryOp      {underscoreToCase} with prefix = "GCC" deriving (Eq, Show) #}
-- | gcc_jit_comparison
{# enum gcc_jit_comparison    as JITComparison   {underscoreToCase} with prefix = "GCC_JIT_COMPARISON_" add prefix = "JIT" deriving (Eq, Show) #}

-- * Types
-- | gcc_jit_object*, superclass for all other values
{#pointer *gcc_jit_object as JITObject newtype#}

-- | gcc_jit_result*, JIT compilation result
{#pointer *gcc_jit_result as JITResult newtype#}

-- | gcc_jit_context*, a JIT context
{#pointer *gcc_jit_context as JITContext newtype#}

-- | gcc_jit_location*, a location in the source code, used for debug output
{#pointer *gcc_jit_location as JITLocation newtype#}

-- | gcc_jit_type*, a type(ex. unsigned int)
{#pointer *gcc_jit_type as JITType newtype#}

-- | gcc_jit_rvalue*, a value on the right side of an assignment
{#pointer *gcc_jit_rvalue as JITRValue newtype#}

-- | gcc_jit_lvalue*, a value on the left side of an assignment
{#pointer *gcc_jit_lvalue as JITLValue newtype#}

-- | gcc_jit_param*, a function parameter
{#pointer *gcc_jit_param as JITParam newtype#}

-- | gcc_jit_function*, a function, defined or imported
{#pointer *gcc_jit_function as JITFunction newtype#}

-- | gcc_jit_block*, a block inside of a function
{#pointer *gcc_jit_block as JITBlock newtype#}

-- | gcc_jit_field*, a struct
{#pointer *gcc_jit_struct as JITStruct newtype#}

-- | gcc_jit_field*, a struct field
{#pointer *gcc_jit_field as JITField newtype#}

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
-- | gcc_jit_case*, a case within a switch statement
{#pointer *gcc_jit_case as JITCase newtype#}
#endif

-- * Object Functions
-- | gcc_jit_object_get_context
objectGetContext :: JITObject -> IO JITContext
objectGetContext = {#call unsafe gcc_jit_object_get_context#}

-- | gcc_jit_object_get_debug_string, the resulting 'ByteString' will have the same lifetime as the 'JITObject' it was created from
objectGetDebugString :: JITObject -> IO ByteString
objectGetDebugString o = {#call unsafe gcc_jit_object_get_debug_string#} o >>= unsafePackCString

-- * Location Functions
-- | gcc_jit_context_new_location
contextNewLocation :: JITContext -> ByteString -> Int -> Int -> IO JITLocation
contextNewLocation c b l c' = useAsCString b $ \cs -> {#call unsafe gcc_jit_context_new_location#} c cs (fromIntegral l) (fromIntegral c')

-- | Convert Nothing to nullPtr, otherwise unwrap the 'JITLocation'
fromMaybeLocation :: Maybe JITLocation -> JITLocation
fromMaybeLocation = fromMaybe (JITLocation nullPtr)

-- * Type Functions
-- | gcc_jit_context_get_type
contextGetType :: JITContext -> JITTypeName -> IO JITType
contextGetType c t = {#call unsafe gcc_jit_context_get_type#} c $ enumToCInt t

-- | gcc_jit_context_get_int_type
contextGetIntType :: JITContext -> Int -> Bool -> IO JITType
contextGetIntType c nb sgnd = {#call unsafe gcc_jit_context_get_int_type#} c (fromIntegral nb) (boolToCInt sgnd)

-- | gcc_jit_type_get_pointer
typeGetPointer :: JITType -> IO JITType
typeGetPointer = {#call unsafe gcc_jit_type_get_pointer#}

-- | gcc_jit_type_get_const
typeGetConst :: JITType -> IO JITType
typeGetConst = {#call unsafe gcc_jit_type_get_const#}

-- | gcc_jit_type_get_volatile
typeGetVolatile :: JITType -> IO JITType
typeGetVolatile = {#call unsafe gcc_jit_type_get_volatile#}

-- | gcc_jit_context_new_array_type
contextNewArrayType :: JITContext -> Maybe JITLocation -> JITType -> Int -> IO JITType
contextNewArrayType c l t ln = {#call unsafe gcc_jit_context_new_array_type#} c (fromMaybeLocation l) t (fromIntegral ln)

-- | gcc_jit_context_new_union_type
contextNewUnionType :: JITContext -> Maybe JITLocation -> ByteString -> [JITField] -> IO JITType
contextNewUnionType c l n fs = withArray (map extractPointer fs) $ \ar -> useAsCString n $ \cs -> {#call unsafe gcc_jit_context_new_union_type#} c (fromMaybeLocation l) cs (fromIntegral $ length fs) (castPtr ar :: Ptr JITField)
    where
        extractPointer (JITField p) = p --JITParam isn't Storable, but the pointer it encapsulates is

-- | gcc_jit_type_as_object
typeAsObject :: JITType -> IO JITObject
typeAsObject = {#call unsafe gcc_jit_type_as_object#}

-- * Function Functions
-- | gcc_jit_context_new_function
contextNewFunction :: JITContext -> Maybe JITLocation -> JITFunctionKind -> JITType -> ByteString -> [JITParam] -> Bool -> IO JITFunction
contextNewFunction c l k rt n ps isvar = withArray (map extractPointer ps) $
    \ar -> useAsCString n $
        \cs -> {#call unsafe gcc_jit_context_new_function#} c (fromMaybeLocation l) (enumToCInt k) rt cs (fromIntegral $ length ps) (castPtr ar :: Ptr JITParam) (boolToCInt isvar)
    where
        extractPointer (JITParam p) = p --JITParam isn't Storable, but the pointer it encapsulates is

-- | gcc_jit_context_get_builtin_function
contextGetBuiltinFunction :: JITContext -> ByteString -> IO JITFunction
contextGetBuiltinFunction c n = useAsCString n $ {#call unsafe gcc_jit_context_get_builtin_function#} c

-- | gcc_jit_function_as_object
functionAsObject :: JITFunction -> IO JITObject
functionAsObject = {#call unsafe gcc_jit_function_as_object#}

-- TODO move to better location
-- | gcc_jit_param_as_rvalue
paramAsRValue :: JITParam -> IO JITRValue
paramAsRValue = {#call unsafe gcc_jit_param_as_rvalue#}

-- TODO move to better location
-- |  gcc_jit_function_dump_to_dot
functionDumpToDot :: JITFunction -> ByteString -> IO ()
functionDumpToDot f fn = useAsCString fn $ {#call unsafe gcc_jit_function_dump_to_dot#} f

-- * Param Functions
-- | gcc_jit_context_new_param
contextNewParam :: JITContext -> Maybe JITLocation -> JITType -> ByteString -> IO JITParam
contextNewParam c l t n = useAsCString n $ \cs -> {#call unsafe gcc_jit_context_new_param#} c (fromMaybeLocation l) t cs

-- * Block Functions
-- | gcc_jit_function_new_block
functionNewBlock :: JITFunction -> Maybe ByteString -> IO JITBlock
functionNewBlock f n = if isJust n
                           then useAsCString (fromJust n) $ \s -> {#call unsafe gcc_jit_function_new_block#} f s
                           else {#call unsafe gcc_jit_function_new_block#} f nullPtr

-- | gcc_jit_block_as_object
blockAsObject :: JITBlock -> IO JITObject
blockAsObject = {#call unsafe gcc_jit_block_as_object#}

-- | gcc_jit_block_get_function
blockGetFunction :: JITBlock -> IO JITFunction
blockGetFunction = {#call unsafe gcc_jit_block_get_function#}

-- * RValue Functions
-- | gcc_jit_rvalue_as_object
rvalueAsObject :: JITRValue -> IO JITObject
rvalueAsObject = {#call unsafe gcc_jit_rvalue_as_object#}

-- | gcc_jit_context_new_string_literal
contextNewStringLiteral :: JITContext -> ByteString -> IO JITRValue
contextNewStringLiteral c s = useAsCString s $ \cs -> {#call unsafe gcc_jit_context_new_string_literal#} c cs

-- | gcc_jit_context_new_rvalue_from_int
contextNewRValueFromInt :: JITContext -> JITType -> Int -> IO JITRValue
contextNewRValueFromInt c t x = {#call unsafe gcc_jit_context_new_rvalue_from_int#} c t $ fromIntegral x

-- | gcc_jit_context_new_rvalue_from_long
contextNewRValueFromLong :: JITContext -> JITType -> Integer -> IO JITRValue
contextNewRValueFromLong c t x = {#call unsafe gcc_jit_context_new_rvalue_from_long#} c t $ fromIntegral x

-- | gcc_jit_context_new_rvalue_from_double
contextNewRValueFromDouble :: JITContext -> JITType -> Double -> IO JITRValue
contextNewRValueFromDouble c t x = {#call unsafe gcc_jit_context_new_rvalue_from_double#} c t $ realToFrac x

-- | gcc_jit_context_zero
contextZero :: JITContext -> JITType -> IO JITRValue
contextZero = {#call unsafe gcc_jit_context_zero#}

-- | gcc_jit_context_one
contextOne :: JITContext -> JITType -> IO JITRValue
contextOne = {#call unsafe gcc_jit_context_one#}

-- | gcc_jit_context_null
contextNull :: JITContext -> JITType -> IO JITRValue
contextNull = {#call unsafe gcc_jit_context_null#}

-- * LValue Functions
-- | gcc_jit_function_new_local
functionNewLocal :: JITFunction -> Maybe JITLocation -> JITType -> ByteString -> IO JITLValue
functionNewLocal f l t n = useAsCString n $ \cs -> {#call unsafe gcc_jit_function_new_local#} f (fromMaybeLocation l) t cs

-- | gcc_jit_context_new_global
contextNewGlobal :: JITContext -> Maybe JITLocation -> JITGlobalKind -> JITType -> ByteString -> IO JITLValue
contextNewGlobal c l k t n = useAsCString n $ \cs -> {#call unsafe gcc_jit_context_new_global#} c (fromMaybeLocation l) (enumToCInt k) t cs

-- | gcc_jit_lvalue_as_object
lvalueAsObject :: JITLValue -> IO JITObject
lvalueAsObject = {#call unsafe gcc_jit_lvalue_as_object#}

-- | gcc_jit_lvalue_as_rvalue
lvalueAsRValue :: JITLValue -> IO JITRValue
lvalueAsRValue = {#call unsafe gcc_jit_lvalue_as_rvalue#}

-- * Struct Functions
-- | gcc_jit_context_new_struct_type
contextNewStructType :: JITContext -> Maybe JITLocation -> ByteString -> [JITField] -> IO JITStruct
contextNewStructType c l n fs = withArray (map extractPointer fs) $ \ar -> useAsCString n $ \cs -> {#call unsafe gcc_jit_context_new_struct_type#} c (fromMaybeLocation l) cs (fromIntegral $ length fs) (castPtr ar :: Ptr JITField)
    where
        extractPointer (JITField p) = p --JITParam isn't Storable, but the pointer it encapsulates is

-- | gcc_jit_context_new_opaque_struct
contextNewOpaqueStruct :: JITContext -> Maybe JITLocation -> ByteString -> IO JITStruct
contextNewOpaqueStruct c l n = useAsCString n $ \cs -> {#call unsafe gcc_jit_context_new_opaque_struct#} c (fromMaybeLocation l) cs

-- | gcc_jit_struct_set_fields
structSetFields :: JITStruct -> Maybe JITLocation -> [JITField] -> IO ()
structSetFields s l fs = withArray (map extractPointer fs) $ \ar -> {#call unsafe gcc_jit_struct_set_fields#} s (fromMaybeLocation l) (fromIntegral $ length fs) (castPtr ar :: Ptr JITField)
    where
        extractPointer (JITField p) = p --JITParam isn't Storable, but the pointer it encapsulates is

-- | gcc_jit_struct_as_type
structAsType :: JITStruct -> IO JITType
structAsType = {#call unsafe gcc_jit_struct_as_type#}

-- * Field Functions
-- | gcc_jit_context_new_field
contextNewField :: JITContext -> Maybe JITLocation -> JITType -> ByteString -> IO JITField
contextNewField c l t n = useAsCString n $ \cs -> {#call unsafe gcc_jit_context_new_field#} c (fromMaybeLocation l) t cs

-- | gcc_jit_field_as_object
fieldAsObject :: JITField -> IO JITObject
fieldAsObject = {#call unsafe gcc_jit_field_as_object#}


#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
-- * Case Functions
-- | gcc_jit_context_new_case
contextNewCase :: JITContext -> JITRValue -> JITRValue -> JITBlock -> IO JITCase
contextNewCase = {#call unsafe gcc_jit_context_new_case#}

-- | gcc_jit_case_as_object
caseAsObject :: JITCase -> IO JITObject
caseAsObject = {#call unsafe gcc_jit_case_as_object#}
#endif

-- The created C bindings are inserted at the end of the file
-- * C functions
