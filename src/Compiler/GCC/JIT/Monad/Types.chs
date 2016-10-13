#include <libgccjit.h>

module Compiler.GCC.JIT.Monad.Types (
    JITState,
    JIT,
    evalJIT,
    evalJITState,
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

    location,
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

type JITState s a = StateT s (StateT JITContext IO) a

type JIT a = JITState () a
-- | Returns the current context
context :: JITState s JITContext
context = lift get

-- | Run the JIT monad under the given context
evalJIT :: JITContext -> JIT a -> IO a
evalJIT ctx j = fst <$> runStateT (fst <$> runStateT j ()) ctx

-- | Run the JIT monad under the given context
evalJITState :: JITContext -> s -> JITState s a -> IO a
evalJITState ctx s j = fst <$> runStateT (fst <$> runStateT j s) ctx

class AsObject a where
    asObject :: a -> JITState s JITObject

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
    asRValue :: a -> JITState s JITRValue

instance AsRValue JITParam where
    asRValue = liftIO1 paramAsRValue
instance AsRValue JITLValue where
    asRValue = liftIO1 lvalueAsRValue

class Releaseable a where
    release :: a -> JITState s ()

#ifdef LIBGCCJIT_HAVE_TIMING_API
instance Releaseable JITTimer where
    release = liftIO1 timerRelease
#endif
instance Releaseable JITContext where
    release = liftIO1 contextRelease
instance Releaseable JITResult where
    release = liftIO1 resultRelease

class AsType a where
    asType :: a -> JITState s JITType

instance AsType JITStruct where
    asType = liftIO1 structAsType

instance Show JITObject where
    show = unpack . unsafePerformIO . objectGetDebugString --Yeah, I'm a bad person.

-- * Location functions
-- | create a new location
location :: ByteString -- ^ File
         -> Int -- ^ Line
         -> Int -- ^ Column
         -> JITState s JITLocation
location fn l c = context >>= \c' -> liftIO $ contextNewLocation c' fn l c

-- * Struct Functions
-- | gcc_jit_context_new_struct_type
structType :: Maybe JITLocation -> ByteString -> [JITField] -> JITState s JITStruct
structType l n fs = context >>= \c -> liftIO $ contextNewStructType c l n fs

-- | gcc_jit_context_new_opaque_struct
opaqueStruct :: Maybe JITLocation -> ByteString -> JITState s JITStruct
opaqueStruct l n = context >>= \c -> liftIO $ contextNewOpaqueStruct c l n

-- | gcc_jit_struct_set_fields
setStructFields :: JITStruct -> Maybe JITLocation -> [JITField] -> JITState s ()
setStructFields s l fs = liftIO $ structSetFields s l fs

-- * Field Functions
-- | gcc_jit_context_new_field
field :: Maybe JITLocation -> JITType -> ByteString -> JITState s JITField
field l t n = context >>= \c -> liftIO $ contextNewField c l t n

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
case' :: JITRValue -> JITRValue -> JITBlock -> JITState s JITCase
case' r r' b = context >>= \c -> liftIO $ contextNewCase c r r' b
#endif

-- Utilities, reimplemented because of an import cycle
liftIO1 :: (a -> IO b) -> a -> JITState s b
liftIO1 f x = liftIO $ f x
