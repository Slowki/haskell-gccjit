module Compiler.GCC.JIT.Monad.Expression where

#include <libgccjit.h>

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign hiding (rvalueGetType, rvalueAccessField, lvalueGetAddress, lvalueAccessField)
import qualified Compiler.GCC.JIT.Foreign as F (rvalueGetType, rvalueAccessField, lvalueGetAddress, lvalueAccessField)

import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- * Expression functions
binaryOp :: Maybe JITLocation -> JITBinaryOp -> JITType -> JITRValue -> JITRValue -> JIT JITRValue
binaryOp = inContext5 contextNewBinaryOp

unaryOp :: Maybe JITLocation -> JITUnaryOp -> JITType -> JITRValue -> JIT JITRValue
unaryOp = inContext4 contextNewUnaryOp

comparison :: Maybe JITLocation -> JITComparison -> JITRValue -> JITRValue -> JIT JITRValue
comparison = inContext4 contextNewComparison

call :: Maybe JITLocation -> JITFunction -> [JITRValue] -> JIT JITRValue
call = inContext3 contextNewCall

callPtr :: Maybe JITLocation -> JITRValue -> [JITRValue] -> JIT JITRValue
callPtr = inContext3 contextNewCallThroughPtr

cast :: Maybe JITLocation -> JITRValue -> JITType -> JIT JITRValue
cast = inContext3 contextNewCast

arrayAccess :: Maybe JITLocation -> JITRValue -> JITRValue -> JIT JITLValue
arrayAccess = inContext3 contextNewArrayAccess

-- * 'JITRValue' functions

#ifdef LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call
setRequireTailCall :: JITRValue -> Bool -> JIT ()
setRequireTailCall = liftIO2 rvalueSetBoolRequireTailCall
#endif

-- | gcc_jit_rvalue_get_type
rvalueGetType :: JITRValue -> JIT JITType
rvalueGetType = liftIO1 F.rvalueGetType

-- | gcc_jit_rvalue_dereference
dereference :: JITRValue -> Maybe JITLocation -> JIT JITLValue
dereference = liftIO2 rvalueDereference

-- | gcc_jit_rvalue_access_field
rvalueAccessField :: JITRValue -> Maybe JITLocation -> JITField -> JIT JITRValue
rvalueAccessField = liftIO3 F.rvalueAccessField

-- | gcc_jit_rvalue_dereference_field
dereferenceField :: JITRValue -> Maybe JITLocation -> JITField -> JIT JITLValue
dereferenceField = liftIO3 rvalueDereferenceField

-- * 'JITLValue' functions

lvalueGetAddress :: JITLValue -> Maybe JITLocation -> JIT JITRValue
lvalueGetAddress = liftIO2 F.lvalueGetAddress

lvalueAccessField :: JITLValue -> Maybe JITLocation -> JITField -> JIT JITLValue
lvalueAccessField = liftIO3 F.lvalueAccessField

-- * Literal functions
rvalueFromInt :: JITType -> Int -> JIT JITRValue
rvalueFromInt = inContext2 contextNewRValueFromInt

rvalueFromLong :: JITType -> Integer -> JIT JITRValue
rvalueFromLong = inContext2 contextNewRValueFromLong

rvalueFromDouble :: JITType -> Double -> JIT JITRValue
rvalueFromDouble = inContext2 contextNewRValueFromDouble

string :: ByteString -> JIT JITRValue
string = inContext1 contextNewStringLiteral

zero :: JITType -> JIT JITRValue
zero = inContext1 contextZero

one :: JITType -> JIT JITRValue
one = inContext1 contextOne

null :: JITType -> JIT JITRValue
null = inContext1 contextNull
