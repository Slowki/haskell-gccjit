{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.GCC.JIT.Foreign where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.ByteString (ByteString, useAsCString)
import Data.Maybe

#include <libgccjit.h>
{# context lib = "libgccjit" #}

-- * Enums
-- | gcc_jit_types
{# enum gcc_jit_types         as JITTypeName     {underscoreToCase} with prefix = "GCC_JIT_TYPE_" add prefix = "JIT" deriving (Eq, Show) #}
-- | gcc_jit_function_kind
{# enum gcc_jit_function_kind as JITFunctionKind {underscoreToCase} with prefix = "GCC_" deriving (Eq, Show) #}
-- | gcc_jit_global_kind
{# enum gcc_jit_global_kind   as JITGlobalKind   {underscoreToCase} with prefix = "GCC_" deriving (Eq, Show) #}
-- | gcc_jit_int_option
{# enum gcc_jit_int_option    as JITIntOption    {underscoreToCase} with prefix = "GCC_JIT_INT_OPTION" add prefix = "JIT" deriving (Eq, Show) #}
-- | gcc_jit_bool_option
{# enum gcc_jit_bool_option   as JITBoolOption   {underscoreToCase} with prefix = "GCC_JIT_BOOL_OPTION" add prefix = "JIT" deriving (Eq, Show) #}
-- | gcc_jit_binary_op
{# enum gcc_jit_binary_op     as JITBinaryOp     {underscoreToCase} with prefix = "GCC_JIT_BINARY_" add prefix = "JIT" deriving (Eq, Show) #}
-- | gcc_jit_unary_op
{# enum gcc_jit_unary_op      as JITUnaryOp      {underscoreToCase} with prefix = "GCC" deriving (Eq, Show) #}
-- | gcc_jit_comparison
{# enum gcc_jit_comparison    as JITComparison   {underscoreToCase} with prefix = "GCC_JIT_COMPARISON_" add prefix = "JIT" deriving (Eq, Show) #}

-- * Types
-- | gcc_jit_result*
{#pointer *gcc_jit_result as JITResult newtype#}

-- | gcc_jit_context*
{#pointer *gcc_jit_context as JITContext newtype#}

-- | gcc_jit_location*
{#pointer *gcc_jit_location as JITLocation newtype#}

-- | gcc_jit_type*
{#pointer *gcc_jit_type as JITType newtype#}

-- | gcc_jit_function*
{#pointer *gcc_jit_function as JITFunction newtype#}

-- | gcc_jit_block*
{#pointer *gcc_jit_block as JITBlock newtype#}

-- | gcc_jit_param*
{#pointer *gcc_jit_param as JITParam newtype#}

-- | gcc_jit_rvalue*
{#pointer *gcc_jit_rvalue as JITRValue newtype#}

-- | gcc_jit_lvalue*
{#pointer *gcc_jit_lvalue as JITLValue newtype#}

-- | gcc_jit_object*
{#pointer *gcc_jit_object as JITObject newtype#}

-- | gcc_jit_object*
{#pointer *gcc_jit_field as JITField newtype#}

-- * Result functions

-- | gcc_jit_context_compile
contextCompile :: JITContext -> IO JITResult
contextCompile = {#call unsafe gcc_jit_context_compile#}

-- | gcc_jit_result_get_code
resultGetCode :: JITResult -> ByteString -> IO (FunPtr a)
resultGetCode r n = castPtrToFunPtr <$> (useAsCString n $ actual r)
    where
        actual = {#call unsafe gcc_jit_result_get_code#}

resultRelease :: JITResult -> IO ()
resultRelease = {#call unsafe gcc_jit_result_release#}

-- * Context functions

-- | gcc_jit_context_acquire
contextAquire :: IO JITContext
contextAquire = {#call unsafe gcc_jit_context_acquire#}

-- | gcc_jit_context_new_child_context
newChildJITContext :: JITContext -> IO JITContext
newChildJITContext = {#call unsafe gcc_jit_context_new_child_context#}

-- | gcc_jit_context_release
contextRelease :: JITContext -> IO ()
contextRelease = {#call unsafe gcc_jit_context_release#}

-- | gcc_jit_context_set_int_option
contextSetIntOption :: JITContext -> JITIntOption -> Int -> IO ()
contextSetIntOption c e v = actual c (fromIntegral $ fromEnum e) (fromIntegral v)
    where
        actual = {#call unsafe gcc_jit_context_set_int_option#}

-- | gcc_jit_context_set_bool_option
contextSetBoolOption :: JITContext -> JITBoolOption -> Bool -> IO ()
contextSetBoolOption c e v = actual c (fromIntegral $ fromEnum e) (if v then 1 else 0)
    where
        actual = {#call unsafe gcc_jit_context_set_bool_option#}

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks
-- | gcc_jit_context_set_bool_allow_unreachable_blocks
contextAllowUnreachableBlocks :: JITContext -> Bool -> IO ()
contextAllowUnreachableBlocks c v = actual c (if v then 1 else 0)
    where
        actual = {#call unsafe gcc_jit_context_set_bool_allow_unreachable_blocks#}
#endif

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver
-- | gcc_jit_context_set_bool_use_external_driver
contextUseExternalDriver :: JITContext -> Bool -> IO ()
contextUseExternalDriver c v = actual c (if v then 1 else 0)
    where
        actual = {#call unsafe gcc_jit_context_set_bool_use_external_driver#}
#endif

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
-- | gcc_jit_context_add_command_line_option
contextAddCommandLineOption :: JITContext -> ByteString -> IO ()
contextAddCommandLineOption c b = useAsCString b $ \s -> actual c s
    where
        actual = {#call unsafe gcc_jit_context_add_command_line_option#}
#endif

-- * Type functions

-- | gcc_jit_context_get_type
contextGetType :: JITContext -> JITTypeName -> IO JITType
contextGetType c t = actual c (fromIntegral $ fromEnum t)
    where
        actual = {#call unsafe gcc_jit_context_get_type#}

-- * Global functions

-- | gcc_jit_context_new_global
contextNewGlobal :: JITContext -> Maybe JITLocation -> JITGlobalKind -> JITType -> ByteString -> IO JITLValue
contextNewGlobal c l k t n = useAsCString n $ \cs -> actual c (fromMaybe (JITLocation nullPtr) l) (fromIntegral $ fromEnum k) t cs
    where
        actual = {#call unsafe gcc_jit_context_new_global#}

-- * Function functions

-- | gcc_jit_context_new_function
contextNewFunction :: JITContext -> Maybe JITLocation -> JITFunctionKind -> JITType -> ByteString -> [JITParam] -> Bool -> IO JITFunction
contextNewFunction c l k rt n ps isvar = withArray (map extractPointer ps) $
    \ar -> useAsCString n $
        \cs -> actual c (fromMaybe (JITLocation nullPtr) l) (fromIntegral $ fromEnum k) rt cs (fromIntegral $ length ps) (castPtr ar :: Ptr JITParam) (if isvar then 1 else 0)
    where
        extractPointer (JITParam p) = p --JITParam isn't Storable, but the pointer it encapsulates is
        actual = {#call unsafe gcc_jit_context_new_function#}

-- | gcc_jit_context_new_param
contextNewParam :: JITContext -> Maybe JITLocation -> JITType -> ByteString -> IO JITParam
contextNewParam c l t n = useAsCString n $ \cs -> actual c (fromMaybe (JITLocation nullPtr) l) t cs
    where
        actual = {#call unsafe gcc_jit_context_new_param#}

-- | gcc_jit_function_new_local
functionNewLocal :: JITFunction -> Maybe JITLocation -> JITType -> ByteString -> IO JITLValue
functionNewLocal f l t n = useAsCString n $ \cs -> actual f (fromMaybe (JITLocation nullPtr) l) t cs
    where
        actual = {#call unsafe gcc_jit_function_new_local#}

-- * Block functions
-- | gcc_jit_function_new_block
functionNewBlock :: JITFunction -> Maybe ByteString -> IO JITBlock
functionNewBlock f n = if isJust n
                           then useAsCString (fromJust n) $ \s -> actual f s
                           else actual f (nullPtr)
    where
        actual = {#call unsafe gcc_jit_function_new_block#}

-- | gcc_jit_block_add_eval
blockAddEval :: JITBlock -> Maybe JITLocation -> JITRValue -> IO ()
blockAddEval b l = actual b (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_block_add_eval#}

-- | gcc_jit_block_add_assignment
blockAddAssignment :: JITBlock -> Maybe JITLocation -> JITLValue -> JITRValue -> IO ()
blockAddAssignment b l lv rv = actual b (fromMaybe (JITLocation nullPtr) l) lv rv
    where
        actual = {#call unsafe gcc_jit_block_add_assignment#}

-- | gcc_jit_block_end_with_void_return
blockEndWithVoidReturn :: JITBlock -> Maybe JITLocation -> IO ()
blockEndWithVoidReturn b l = actual b (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_block_end_with_void_return#}

-- | gcc_jit_block_end_with_return
blockEndWithReturn :: JITBlock -> Maybe JITLocation -> JITRValue -> IO ()
blockEndWithReturn b l = actual b (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_block_end_with_return#}

-- * Expression functions

-- | gcc_jit_context_new_binary_op
contextNewBinaryOp :: JITContext -> Maybe JITLocation -> JITBinaryOp -> JITType -> JITRValue -> JITRValue -> IO JITRValue
contextNewBinaryOp c l t = actual c (fromMaybe (JITLocation nullPtr) l) (fromIntegral $ fromEnum t)
    where
        actual = {#call unsafe gcc_jit_context_new_binary_op#}

-- | gcc_jit_context_new_unary_op
contextNewUnaryOp :: JITContext -> Maybe JITLocation -> JITUnaryOp -> JITType -> JITRValue -> IO JITRValue
contextNewUnaryOp c l t = actual c (fromMaybe (JITLocation nullPtr) l) (fromIntegral $ fromEnum t)
    where
        actual = {#call unsafe gcc_jit_context_new_unary_op#}

-- | gcc_jit_context_new_comparison
contextNewComparison :: JITContext -> Maybe JITLocation -> JITComparison -> JITType -> JITRValue -> IO JITRValue
contextNewComparison c l t = actual c (fromMaybe (JITLocation nullPtr) l) (fromIntegral $ fromEnum t)
    where
        actual = {#call unsafe gcc_jit_context_new_unary_op#}
-- | gcc_jit_context_new_call
contextNewCall :: JITContext -> Maybe JITLocation -> JITFunction -> [JITRValue] -> IO JITRValue
contextNewCall c l f as = withArray (map extractPointer as) $ \ar -> actual c (fromMaybe (JITLocation nullPtr) l) f (fromIntegral $ length as) (castPtr ar :: Ptr JITRValue)
    where
        extractPointer (JITRValue p) = p --JITRValue isn't Storable, but the pointer it encapsulates is
        actual = {#call unsafe gcc_jit_context_new_call#}

-- | gcc_jit_context_new_call_through_ptr
contextNewCallThroughPtr :: JITContext -> Maybe JITLocation -> JITRValue -> [JITRValue] -> IO JITRValue
contextNewCallThroughPtr c l fp as = withArray (map extractPointer as) $ \ar -> actual c (fromMaybe (JITLocation nullPtr) l) fp (fromIntegral $ length as) (castPtr ar :: Ptr JITRValue)
    where
        extractPointer (JITRValue p) = p --JITRValue isn't Storable, but the pointer it encapsulates is
        actual = {#call unsafe gcc_jit_context_new_call_through_ptr#}

-- | gcc_jit_context_new_cast
contextNewCast :: JITContext -> Maybe JITLocation -> JITRValue -> JITType -> IO JITRValue
contextNewCast c l = actual c (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_context_new_cast#}

-- | gcc_jit_context_new_array_access
contextNewArrayAccess :: JITContext -> Maybe JITLocation -> JITRValue -> JITRValue -> IO JITLValue
contextNewArrayAccess r l = actual r (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_context_new_array_access#}

-- | gcc_jit_param_as_rvalue
_paramAsRValue :: JITParam -> IO JITRValue
_paramAsRValue = {#call unsafe gcc_jit_param_as_rvalue#}

-- * 'JITRValue' functions

#ifdef LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call
-- | gcc_jit_rvalue_set_bool_require_tail_call
rvalueSetBoolRequireTailCall :: JITRValue -> Bool -> IO ()
rvalueSetBoolRequireTailCall r v = actual r (if v then 1 else 0)
    where
        actual = {#call unsafe gcc_jit_rvalue_set_bool_require_tail_call#}
#endif

-- | gcc_jit_rvalue_get_type
rvalueGetType :: JITRValue -> IO JITType
rvalueGetType = {#call unsafe gcc_jit_rvalue_get_type#}

-- | gcc_jit_rvalue_as_object
rvalueAsObject :: JITRValue -> IO JITObject
rvalueAsObject = {#call unsafe gcc_jit_rvalue_as_object#}

-- | gcc_jit_rvalue_dereference
rvalueDereference :: JITRValue -> Maybe JITLocation -> IO JITLValue
rvalueDereference r l = actual r (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_rvalue_dereference#}

-- | gcc_jit_rvalue_access_field
rvalueAccessField :: JITRValue -> Maybe JITLocation -> JITField -> IO JITRValue
rvalueAccessField r l = actual r (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_rvalue_access_field#}

-- | gcc_jit_rvalue_dereference_field
rvalueDereferenceField :: JITRValue -> Maybe JITLocation -> JITField -> IO JITLValue
rvalueDereferenceField r l = actual r (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_rvalue_dereference_field#}

-- * 'JITLValue' functions

-- | gcc_jit_lvalue_as_object
lvalueAsObject :: JITLValue -> IO JITObject
lvalueAsObject = {#call unsafe gcc_jit_lvalue_as_object#}

-- | gcc_jit_lvalue_as_rvalue
lvalueAsRValue :: JITLValue -> IO JITRValue
lvalueAsRValue = {#call unsafe gcc_jit_lvalue_as_rvalue#}

-- | gcc_jit_lvalue_get_address
lvalueGetAddress :: JITLValue -> Maybe JITLocation -> IO JITRValue
lvalueGetAddress v l = actual v (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_lvalue_get_address#}

-- | gcc_jit_lvalue_access_field
lvalueAccessField :: JITLValue -> Maybe JITLocation -> JITField -> IO JITLValue
lvalueAccessField v l = actual v (fromMaybe (JITLocation nullPtr) l)
    where
        actual = {#call unsafe gcc_jit_lvalue_access_field#}

-- * Literal functions
-- | gcc_jit_context_new_string_literal
contextNewStringLiteral :: JITContext -> ByteString -> IO JITRValue
contextNewStringLiteral c s = useAsCString s $ \cs -> actual c cs
    where
        actual = {#call unsafe gcc_jit_context_new_string_literal#}

-- | gcc_jit_context_new_rvalue_from_int
contextNewRValueFromInt :: JITContext -> JITType -> Int -> IO JITRValue
contextNewRValueFromInt c t x = actual c t (fromIntegral x)
    where
        actual = {#call unsafe gcc_jit_context_new_rvalue_from_int#}

-- | gcc_jit_context_new_rvalue_from_long
contextNewRValueFromLong :: JITContext -> JITType -> Integer -> IO JITRValue
contextNewRValueFromLong c t x = actual c t (fromIntegral x)
    where
        actual = {#call unsafe gcc_jit_context_new_rvalue_from_long#}

-- | gcc_jit_context_new_rvalue_from_double
contextNewRValueFromDouble :: JITContext -> JITType -> Double -> IO JITRValue
contextNewRValueFromDouble c t x = actual c t (realToFrac x)
    where
        actual = {#call unsafe gcc_jit_context_new_rvalue_from_double#}

-- | gcc_jit_context_zero
contextZero :: JITContext -> JITType -> IO JITRValue
contextZero = {#call unsafe gcc_jit_context_zero#}

-- | gcc_jit_context_one
contextOne :: JITContext -> JITType -> IO JITRValue
contextOne = {#call unsafe gcc_jit_context_one#}

-- | gcc_jit_context_null
contextNull :: JITContext -> JITType -> IO JITRValue
contextNull = {#call unsafe gcc_jit_context_null#}

-- The created C bindings are inserted at the end of the file
-- * C functions
