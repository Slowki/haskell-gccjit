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

timer :: JITState s JITTimer
timer = liftIO timerNew

setTimer :: JITTimer -> JITState s ()
setTimer = inContext1 contextSetTimer

getTimer :: JITState s JITTimer
getTimer = inContext contextGetTimer

pushTimer :: JITTimer -> ByteString -> JITState s ()
pushTimer = liftIO2 timerPush

popTimer :: JITTimer -> Maybe ByteString -> JITState s ()
popTimer = liftIO2 timerPop

printTimer :: JITTimer -> Handle -> JITState s ()
printTimer = liftIO2 timerPrint

#endif
