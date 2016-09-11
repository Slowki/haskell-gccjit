{-# LANGUAGE ForeignFunctionInterface #-}
-- * Types
module Compiler.GCC.JIT.Foreign.Types where

#include <libgccjit.h>
{# context lib = "libgccjit" #}

-- * Enums
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
