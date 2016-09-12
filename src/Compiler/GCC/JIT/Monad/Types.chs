#include <libgccjit.h>

module Compiler.GCC.JIT.Monad.Types (
    JIT,
    evalJIT,
    context,

    JITTypeName(..),
    JITOutputKind(..),
    JITFunctionKind(..),
    JITGlobalKind(..),
    JITStrOption(..),
    JITIntOption(..),
    JITBoolOption(..),
    JITBinaryOp(..),
    JITUnaryOp(..),
    JITComparison(..),

    JITObject(..),
    JITResult(..),
    JITContext(..),
    JITLocation(..),
    JITType(..),
    JITRValue(..),
    JITLValue(..),
    JITParam(..),
    JITFunction(..),
    JITBlock(..),
    JITStruct(..),
    JITField(..),
#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
    JITCase(..),
#endif
#ifdef LIBGCCJIT_HAVE_TIMING_API
    JITTimer(..),
#endif

    AsObject(..),
    asObject,
    AsRValue(..),
    asRValue,
    Releaseable(..),
    release,
    AsType(..),
    asType,

    structType,
    opaqueStruct,
    setStructFields,
    field,
#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
    case'
#endif

    ) where

import Compiler.GCC.JIT.Foreign.Types
import Compiler.GCC.JIT.Foreign.Context
import Compiler.GCC.JIT.Foreign.Timer

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad

type JIT a = StateT JITContext IO a

-- | Returns the current context
context :: JIT JITContext
context = get

-- | Run the JIT monad under the given context
evalJIT :: JITContext -> JIT a -> IO a
evalJIT ctx j = fst <$> runStateT j ctx

class AsObject a where
    asObject :: a -> JIT JITObject

instance AsObject JITType where
    asObject = liftIO1 typeAsObject
instance AsObject JITRValue where
    asObject = liftIO1 rvalueAsObject
instance AsObject JITLValue where
    asObject = liftIO1 lvalueAsObject
instance AsObject JITFunction where
    asObject = liftIO1 functionAsObject
instance AsObject JITBlock where
    asObject = liftIO1 blockAsObject
instance AsObject JITField where
    asObject = liftIO1 fieldAsObject

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
instance AsObject JITCase where
    asObject = liftIO1 caseAsObject
#endif

class AsRValue a where
    asRValue :: a -> JIT JITRValue

instance AsRValue JITParam where
    asRValue = liftIO1 paramAsRValue
instance AsRValue JITLValue where
    asRValue = liftIO1 lvalueAsRValue

class Releaseable a where
    release :: a -> JIT ()

#ifdef LIBGCCJIT_HAVE_TIMING_API
instance Releaseable JITTimer where
    release = liftIO1 timerRelease
#endif
instance Releaseable JITContext where
    release = liftIO1 contextRelease
instance Releaseable JITResult where
    release = liftIO1 resultRelease

class AsType a where
    asType :: a -> JIT JITType

instance AsType JITStruct where
    asType = liftIO1 structAsType

instance Show JITObject where
    show = unpack . unsafePerformIO . objectGetDebugString --Yeah, I'm a bad person.

-- * Struct Functions
-- | gcc_jit_context_new_struct_type
structType :: Maybe JITLocation -> ByteString -> [JITField] -> JIT JITStruct
structType l n fs = context >>= \c -> liftIO $ contextNewStructType c l n fs

-- | gcc_jit_context_new_opaque_struct
opaqueStruct :: Maybe JITLocation -> ByteString -> JIT JITStruct
opaqueStruct l n = context >>= \c -> liftIO $ contextNewOpaqueStruct c l n

-- | gcc_jit_struct_set_fields
setStructFields :: JITStruct -> Maybe JITLocation -> [JITField] -> JIT ()
setStructFields s l fs = liftIO $ structSetFields s l fs

-- * Field Functions
-- | gcc_jit_context_new_field
field :: Maybe JITLocation -> JITType -> ByteString -> JIT JITField
field l t n = context >>= \c -> liftIO $ contextNewField c l t n

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
case' :: JITRValue -> JITRValue -> JITBlock -> JIT JITCase
case' r r' b = context >>= \c -> liftIO $ contextNewCase c r r' b
#endif

-- Utilities, reimplemented because of an import cycle
liftIO1 :: (a -> IO b) -> a -> JIT b
liftIO1 f x = liftIO $ f x
