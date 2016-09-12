module Compiler.GCC.JIT.Monad.Block where

#include <libgccjit.h>

import Compiler.GCC.JIT.Monad.Utilities
import Compiler.GCC.JIT.Monad.Types

import Compiler.GCC.JIT.Foreign.Types
import Compiler.GCC.JIT.Foreign.Block

import Data.ByteString (ByteString)

-- * 'JITBlock' functions

-- | Create a block
block :: JITFunction
      -> Maybe ByteString -- ^ Block name
      -> JIT JITBlock
block = liftIO2 functionNewBlock

-- | Get a block's parent 'JITFunction'
getBlockFunction :: JITBlock -> JIT JITFunction
getBlockFunction = liftIO1 blockGetFunction

-- | Evaluate an expression
addEval :: JITBlock -> Maybe JITLocation -> JITRValue -> JIT ()
addEval = liftIO3 blockAddEval

-- | Set a 'JITLValue'
addAssignment :: JITBlock -> Maybe JITLocation -> JITLValue -> JITRValue -> JIT ()
addAssignment = liftIO4 blockAddAssignment

-- | Set a 'JITLValue' by applying a 'JITBinaryOp' with a 'JITRValue', roughly equivalent to +=, *=, etc
addAssignmentOp :: JITBlock -> Maybe JITLocation -> JITLValue -> JITBinaryOp -> JITRValue -> JIT ()
addAssignmentOp = liftIO5 blockAddAssignmentOp

addComment :: JITBlock -> Maybe JITLocation -> ByteString -> JIT ()
addComment = liftIO3 blockAddComment

-- * Block End With functions
endWithConditional :: JITBlock
                   -> Maybe JITLocation
                   -> JITRValue -- ^ Condition
                   -> JITBlock -- ^ True block
                   -> JITBlock -- ^ False block
                   -> JIT ()
endWithConditional = liftIO5 blockEndWithConditional

endWithJump :: JITBlock
            -> Maybe JITLocation
            -> JITBlock -- ^ Block to jump to
            -> JIT ()
endWithJump = liftIO3 blockEndWithJump

endWithReturn :: JITBlock
              -> Maybe JITLocation
              -> JITRValue -- ^ Value to return
              -> JIT ()
endWithReturn = liftIO3 blockEndWithReturn

endWithVoidReturn :: JITBlock -> Maybe JITLocation -> JIT ()
endWithVoidReturn = liftIO2 blockEndWithVoidReturn

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
endWithSwitch :: JITBlock -> Maybe JITLocation -> JITRValue -> JITBlock -> [JITCase] -> JIT ()
endWithSwitch = liftIO5 blockEndWithSwitch
#endif
