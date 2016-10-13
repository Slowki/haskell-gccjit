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
      -> JITState s JITBlock
block = liftIO2 functionNewBlock

-- | Get a block's parent 'JITFunction'
getBlockFunction :: JITBlock -> JITState s JITFunction
getBlockFunction = liftIO1 blockGetFunction

-- | Evaluate an expression
addEval :: JITBlock -> Maybe JITLocation -> JITRValue -> JITState s ()
addEval = liftIO3 blockAddEval

-- | Set a 'JITLValue'
addAssignment :: JITBlock -> Maybe JITLocation -> JITLValue -> JITRValue -> JITState s ()
addAssignment = liftIO4 blockAddAssignment

-- | Set a 'JITLValue' by applying a 'JITBinaryOp' with a 'JITRValue', roughly equivalent to +=, *=, etc
addAssignmentOp :: JITBlock -> Maybe JITLocation -> JITLValue -> JITBinaryOp -> JITRValue -> JITState s ()
addAssignmentOp = liftIO5 blockAddAssignmentOp

addComment :: JITBlock -> Maybe JITLocation -> ByteString -> JITState s ()
addComment = liftIO3 blockAddComment

-- * Block End With functions
endWithConditional :: JITBlock
                   -> Maybe JITLocation
                   -> JITRValue -- ^ Condition
                   -> JITBlock -- ^ True block
                   -> JITBlock -- ^ False block
                   -> JITState s ()
endWithConditional = liftIO5 blockEndWithConditional

endWithJump :: JITBlock
            -> Maybe JITLocation
            -> JITBlock -- ^ Block to jump to
            -> JITState s ()
endWithJump = liftIO3 blockEndWithJump

endWithReturn :: JITBlock
              -> Maybe JITLocation
              -> JITRValue -- ^ Value to return
              -> JITState s ()
endWithReturn = liftIO3 blockEndWithReturn

endWithVoidReturn :: JITBlock -> Maybe JITLocation -> JITState s ()
endWithVoidReturn = liftIO2 blockEndWithVoidReturn

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
endWithSwitch :: JITBlock -> Maybe JITLocation -> JITRValue -> JITBlock -> [JITCase] -> JITState s ()
endWithSwitch = liftIO5 blockEndWithSwitch
#endif
