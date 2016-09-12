{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Compiler.GCC.JIT.Foreign.Timer where

#include <libgccjit.h>
{# context lib = "libgccjit" #}

{#import Compiler.GCC.JIT.Foreign.Types#}

import Compiler.GCC.JIT.Foreign.Utilities

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.ByteString (ByteString, useAsCString)
import Data.ByteString.Unsafe (unsafePackCString)
import Data.Maybe (fromMaybe, isJust, fromJust)

import System.IO (Handle)

#ifdef LIBGCCJIT_HAVE_TIMING_API
-- * Timer Functions
-- | gcc_jit_timer_new
timerNew :: IO JITTimer
timerNew = {#call unsafe gcc_jit_timer_new#}

-- | gcc_jit_timer_release
timerRelease :: JITTimer -> IO ()
timerRelease = {#call unsafe gcc_jit_timer_release#}

-- | gcc_jit_context_set_timer
contextSetTimer :: JITContext -> JITTimer -> IO ()
contextSetTimer = {#call unsafe gcc_jit_context_set_timer#}

-- | gcc_jit_context_get_timer
contextGetTimer :: JITContext -> IO JITTimer
contextGetTimer = {#call unsafe gcc_jit_context_get_timer#}

-- | gcc_jit_timer_push
timerPush :: JITTimer -> ByteString -> IO ()
timerPush t b = useAsCString b $ {#call unsafe gcc_jit_timer_push#} t

-- | gcc_jit_timer_pop
timerPop :: JITTimer -> Maybe ByteString -> IO ()
timerPop t (Just b) = useAsCString b $ {#call unsafe gcc_jit_timer_pop#} t
timerPop t Nothing  = {#call unsafe gcc_jit_timer_pop#} t (castPtr nullPtr :: Ptr CChar)

-- | gcc_jit_timer_print
timerPrint :: JITTimer -> Handle -> IO ()
timerPrint t h = handleToFile h "w" >>= {#call unsafe gcc_jit_timer_print#} t . castPtr
#endif
