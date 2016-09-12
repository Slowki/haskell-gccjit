module Compiler.GCC.JIT.Monad.Result where

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign.Types
import Compiler.GCC.JIT.Foreign.Context

import Foreign.Ptr

import Data.ByteString (ByteString)

import Control.Monad.IO.Class (liftIO)

-- * Result functions

-- | Create a result from the current context
withResult :: (JITResult -> JIT a) -> JIT a
withResult f = do
    rez <- compile
    ret <- f rez
    release rez
    return ret

-- | Compile the current context and return the result, it is recomend to use 'withResult' instead since you will have to manually free the result from this function with 'releaseResult'
compile :: JIT JITResult
compile = inContext contextCompile

-- | gcc_jit_result_get_code
getCode :: JITResult
        -> ByteString -- ^ Function name
        -> JIT (FunPtr a)
getCode = liftIO2 resultGetCode

-- | gcc_jit_result_get_global
getGlobal :: JITResult
          -> ByteString -- ^ Global name
          -> JIT (Ptr a)
getGlobal = liftIO2 resultGetGlobal

-- | Compile the current context to a file
compileToFile :: JITOutputKind -- ^ Output file type
              -> ByteString -- ^ Output file path
              -> JIT ()
compileToFile = inContext2 contextCompileToFile
