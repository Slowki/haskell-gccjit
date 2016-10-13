module Compiler.GCC.JIT.Monad.Expression where

#include <libgccjit.h>

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign hiding (rvalueGetType, rvalueAccessField, lvalueGetAddress, lvalueAccessField)
import qualified Compiler.GCC.JIT.Foreign as F (rvalueGetType, rvalueAccessField, lvalueGetAddress, lvalueAccessField)

import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- * Expression functions
binaryOp :: Maybe JITLocation -> JITBinaryOp -> JITType -> JITRValue -> JITRValue -> JITState s JITRValue
binaryOp = inContext5 contextNewBinaryOp

unaryOp :: Maybe JITLocation -> JITUnaryOp -> JITType -> JITRValue -> JITState s JITRValue
unaryOp = inContext4 contextNewUnaryOp

comparison :: Maybe JITLocation -> JITComparison -> JITRValue -> JITRValue -> JITState s JITRValue
comparison = inContext4 contextNewComparison

call :: Maybe JITLocation -> JITFunction -> [JITRValue] -> JITState s JITRValue
call = inContext3 contextNewCall

callPtr :: Maybe JITLocation -> JITRValue -> [JITRValue] -> JITState s JITRValue
callPtr = inContext3 contextNewCallThroughPtr

cast :: Maybe JITLocation -> JITRValue -> JITType -> JITState s JITRValue
cast = inContext3 contextNewCast

arrayAccess :: Maybe JITLocation -> JITRValue -> JITRValue -> JITState s JITLValue
arrayAccess = inContext3 contextNewArrayAccess

-- * 'JITRValue' functions

#ifdef LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call
setRequireTailCall :: JITRValue -> Bool -> JITState s ()
setRequireTailCall = liftIO2 rvalueSetBoolRequireTailCall
#endif

-- | gcc_jit_rvalue_get_type
rvalueGetType :: JITRValue -> JITState s JITType
rvalueGetType = liftIO1 F.rvalueGetType

-- | gcc_jit_rvalue_dereference
dereference :: JITRValue -> Maybe JITLocation -> JITState s JITLValue
dereference = liftIO2 rvalueDereference

-- | gcc_jit_rvalue_access_field
rvalueAccessField :: JITRValue -> Maybe JITLocation -> JITField -> JITState s JITRValue
rvalueAccessField = liftIO3 F.rvalueAccessField

-- | gcc_jit_rvalue_dereference_field
dereferenceField :: JITRValue -> Maybe JITLocation -> JITField -> JITState s JITLValue
dereferenceField = liftIO3 rvalueDereferenceField

-- * 'JITLValue' functions

lvalueGetAddress :: JITLValue -> Maybe JITLocation -> JITState s JITRValue
lvalueGetAddress = liftIO2 F.lvalueGetAddress

lvalueAccessField :: JITLValue -> Maybe JITLocation -> JITField -> JITState s JITLValue
lvalueAccessField = liftIO3 F.lvalueAccessField

-- * Literal functions
rvalueFromInt :: JITType -> Int -> JITState s JITRValue
rvalueFromInt = inContext2 contextNewRValueFromInt

rvalueFromLong :: JITType -> Integer -> JITState s JITRValue
rvalueFromLong = inContext2 contextNewRValueFromLong

rvalueFromDouble :: JITType -> Double -> JITState s JITRValue
rvalueFromDouble = inContext2 contextNewRValueFromDouble

string :: ByteString -> JITState s JITRValue
string = inContext1 contextNewStringLiteral

zero :: JITType -> JITState s JITRValue
zero = inContext1 contextZero

one :: JITType -> JITState s JITRValue
one = inContext1 contextOne

null :: JITType -> JITState s JITRValue
null = inContext1 contextNull
