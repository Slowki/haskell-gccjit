module Compiler.GCC.JIT.Monad.Type where -- Types.hs and Type.hs, what could possible go wrong

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign.Types
import Compiler.GCC.JIT.Foreign.Block

import Data.ByteString (ByteString)

-- * Type functions
getType :: JITTypeName -> JIT JITType
getType = inContext1 contextGetType

-- | gcc_jit_context_get_int_type
getIntType :: Int -- ^ Size
           -> Bool -- ^ Is signed
           -> JIT JITType
getIntType = inContext2 contextGetIntType

-- | gcc_jit_type_get_pointer
getPointer :: JITType -> JIT JITType
getPointer = liftIO1 typeGetPointer

-- | gcc_jit_type_get_const
getConst :: JITType -> JIT JITType
getConst = liftIO1 typeGetConst

-- | gcc_jit_type_get_volatile
getVolatile :: JITType -> JIT JITType
getVolatile = liftIO1 typeGetVolatile

-- | gcc_jit_context_new_array_type
arrayType :: Maybe JITLocation -> JITType -> Int -> JIT JITType
arrayType = inContext3 contextNewArrayType

-- | gcc_jit_context_new_union_type
unionType :: Maybe JITLocation -> ByteString -> [JITField] -> JIT JITType
unionType = inContext3 contextNewUnionType
