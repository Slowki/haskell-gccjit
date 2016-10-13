module Compiler.GCC.JIT.Monad.Type where -- Types.hs and Type.hs, what could possible go wrong

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign.Types
import Compiler.GCC.JIT.Foreign.Block

import Data.ByteString (ByteString)

-- * Type functions
getType :: JITTypeName -> JITState s JITType
getType = inContext1 contextGetType

-- | gcc_jit_context_get_int_type
getIntType :: Int -- ^ Size
           -> Bool -- ^ Is signed
           -> JITState s JITType
getIntType = inContext2 contextGetIntType

-- | gcc_jit_type_get_pointer
getPointer :: JITType -> JITState s JITType
getPointer = liftIO1 typeGetPointer

-- | gcc_jit_type_get_const
getConst :: JITType -> JITState s JITType
getConst = liftIO1 typeGetConst

-- | gcc_jit_type_get_volatile
getVolatile :: JITType -> JITState s JITType
getVolatile = liftIO1 typeGetVolatile

-- | gcc_jit_context_new_array_type
arrayType :: Maybe JITLocation -> JITType -> Int -> JITState s JITType
arrayType = inContext3 contextNewArrayType

-- | gcc_jit_context_new_union_type
unionType :: Maybe JITLocation -> ByteString -> [JITField] -> JITState s JITType
unionType = inContext3 contextNewUnionType
