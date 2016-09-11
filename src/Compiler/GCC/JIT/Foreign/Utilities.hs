module Compiler.GCC.JIT.Foreign.Utilities where

import Compiler.GCC.JIT.Foreign.Types

import Foreign.C.Types
import Foreign.Ptr

import Data.Maybe

-- | Convert Nothing to nullPtr
fromMaybeLocation :: Maybe JITLocation -> JITLocation
fromMaybeLocation = fromMaybe (JITLocation nullPtr)

-- | Convert enums to their CInt values
enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum

-- | Convert True to 1 and False to 0
boolToCInt :: Bool -> CInt
boolToCInt True  = 1
boolToCInt False = 0
