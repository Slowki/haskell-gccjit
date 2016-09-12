#include <libgccjit.h>

module Compiler.GCC.JIT.Monad.Timer where

#ifdef LIBGCCJIT_HAVE_TIMING_API

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types
import Compiler.GCC.JIT.Foreign.Types (JITTimer(..))
import Compiler.GCC.JIT.Foreign.Timer

import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)

import System.IO (Handle)

timer :: JIT JITTimer
timer = liftIO timerNew

setTimer :: JITTimer -> JIT ()
setTimer = inContext1 contextSetTimer

getTimer :: JIT JITTimer
getTimer = inContext contextGetTimer

pushTimer :: JITTimer -> ByteString -> JIT ()
pushTimer = liftIO2 timerPush

popTimer :: JITTimer -> Maybe ByteString -> JIT ()
popTimer = liftIO2 timerPop

printTimer :: JITTimer -> Handle -> JIT ()
printTimer = liftIO2 timerPrint

#endif
