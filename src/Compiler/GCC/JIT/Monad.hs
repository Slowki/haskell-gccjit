module Compiler.GCC.JIT.Monad where

import Compiler.GCC.JIT.Foreign hiding (paramAsRValue)
import qualified Compiler.GCC.JIT.Foreign as F (paramAsRValue)

import Data.ByteString hiding (null)

import Control.Monad.IO.Class
import Control.Monad.State

type JIT a = StateT JITContext IO a

evalJIT :: JITContext -> JIT a -> IO a
evalJIT ctx j = fst <$> runStateT j ctx

-- * Context functions
withContext :: JIT a -- ^ JIT code to load into the context
            -> (JITResult -> IO b) -- ^ the 'JITResult' will be freed after this function exits
            -> IO b
withContext j f = do
    ctx <- contextAquire
    evalJIT ctx j
    compd <- contextCompile ctx
    r <- f compd
    contextRelease ctx
    resultRelease compd
    return r

context :: JIT JITContext
context = get

setContextBoolOption :: JITBoolOption -> Bool -> JIT ()
setContextBoolOption o v = get >>= \c -> liftIO $ contextSetBoolOption c o v

setContextIntOption :: JITIntOption -> Int -> JIT ()
setContextIntOption o v = get >>= \c -> liftIO $ contextSetIntOption c o v

-- * Type functions
getType :: JITTypeName -> JIT JITType
getType n = get >>= \c -> liftIO $ contextGetType c n

-- * Function functions
function :: Maybe JITLocation -> JITFunctionKind -> JITType -> ByteString -> [JITParam] -> Bool -> JIT JITFunction
function l k t n p v = get >>= \c -> liftIO $ contextNewFunction c l k t n p v

param :: Maybe JITLocation -> JITType -> ByteString -> JIT JITParam
param l t n = get >>= \c -> liftIO $ contextNewParam c l t n

local :: JITFunction -> Maybe JITLocation -> JITType -> ByteString -> JIT JITLValue
local f l t n = liftIO $ functionNewLocal f l t n

-- * Expression functions
binaryOp :: Maybe JITLocation -> JITBinaryOp -> JITType -> JITRValue -> JITRValue -> JIT JITRValue
binaryOp l b t x y = get >>= \c -> liftIO $ contextNewBinaryOp c l b t x y

paramAsRValue :: JITParam -> JIT JITRValue
paramAsRValue p = liftIO $ F.paramAsRValue p

newCall :: Maybe JITLocation -> JITFunction -> [JITRValue] -> JIT JITRValue
newCall l f rs = get >>= \c -> liftIO $ contextNewCall c l f rs

newCallPtr :: Maybe JITLocation -> JITRValue -> [JITRValue] -> JIT JITRValue
newCallPtr l fp rs = get >>= \c -> liftIO $ contextNewCallThroughPtr c l fp rs

stringLiteral :: ByteString -> JIT JITRValue
stringLiteral b = get >>= \c -> liftIO $ contextNewStringLiteral c b

zero :: JITType -> JIT JITRValue
zero t = get >>= \c -> liftIO $ contextZero c t

one :: JITType -> JIT JITRValue
one t = get >>= \c -> liftIO $ contextOne c t

null :: JITType -> JIT JITRValue
null t = get >>= \c -> liftIO $ contextNull c t

-- * Block functions
block :: JITFunction -> Maybe ByteString -> JIT JITBlock
block f n = liftIO $ functionNewBlock f n

addEval :: JITBlock -> Maybe JITLocation -> JITRValue -> JIT ()
addEval b l r = liftIO $ blockAddEval b l r

addAssignment :: JITBlock -> Maybe JITLocation -> JITRValue -> IO ()
addAssignment b l v = liftIO $ blockAddEval b l v

endWithVoidReturn :: JITBlock -> Maybe JITLocation -> JIT ()
endWithVoidReturn b l = liftIO $ blockEndWithVoidReturn b l

endWithReturn :: JITBlock -> Maybe JITLocation -> JITRValue -> JIT ()
endWithReturn b l v = liftIO $ blockEndWithReturn b l v
{-
withChildContext :: JIT a -> JIT a
withChildContext j = do
    ctx <- get
    cCtx <- liftIO $ newChildContext ctx
    r <- liftIO $ evalJIT cCtx j
    liftIO $ contextRelease cCtx
    return r
-}
