module Compiler.GCC.JIT.Monad.Function where

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign.Types
import Compiler.GCC.JIT.Foreign.Block

import Data.ByteString (ByteString)

import Control.Monad.IO.Class (liftIO)

-- * 'JITFunction' functions
function :: Maybe JITLocation
         -> JITFunctionKind -- ^ Kind
         -> JITType -- ^ Return type
         -> ByteString -- ^ Name
         -> [JITParam] -- ^ Parameters
         -> Bool -- ^ Is variadic
         -> JITState s JITFunction
function l k t n p v = context >>= \c -> liftIO $ contextNewFunction c l k t n p v

getBuiltinFunction :: ByteString -- ^ Builtin function name
                   -> JITState s JITFunction
getBuiltinFunction = inContext1 contextGetBuiltinFunction

dumpFunctionToDot :: JITFunction -> ByteString -> JITState s ()
dumpFunctionToDot = liftIO2 functionDumpToDot

-- * 'JITParam' functions
param :: Maybe JITLocation
      -> JITType -- ^ Type
      -> ByteString -- ^ Name
      -> JITState s JITParam
param = inContext3 contextNewParam

-- * Variable functions
-- | Create a new local variable
local :: JITFunction
      -> Maybe JITLocation
      -> JITType -- ^ Type
      -> ByteString -- ^ Name
      -> JITState s JITLValue
local = liftIO4 functionNewLocal

-- | Create a new global variable
global :: Maybe JITLocation
       -> JITGlobalKind -- ^ Variable kind
       -> JITType -- ^ Type
       -> ByteString -- ^ Name
       -> JITState s JITLValue
global l k t n = context >>= \c -> liftIO $ contextNewGlobal c l k t n
