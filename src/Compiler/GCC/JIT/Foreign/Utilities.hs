module Compiler.GCC.JIT.Foreign.Utilities where

import Foreign.C.Types
import Foreign.Ptr

import Data.Maybe

-- | Convert enums to their CInt values
enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum

-- | Convert True to 1 and False to 0
boolToCInt :: Bool -> CInt
boolToCInt True  = 1
boolToCInt False = 0
