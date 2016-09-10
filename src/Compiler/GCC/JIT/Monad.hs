module Compiler.GCC.JIT.Monad where

import Compiler.GCC.JIT.Foreign

import Data.ByteString

import Control.Monad.IO.Class
import Control.Monad.State

type JIT a = StateT JITContext IO a

evalJIT :: JITContext -> JIT a -> IO a
evalJIT ctx j = fst <$> runStateT j ctx

withContext :: JIT a -- ^ JIT code to load into the context
            -> (JITResult -> IO b) -- ^ the 'JITResult' will be freed after this function exits
            -> IO b
withContext j f = do
    ctx <- contextAquire
    evalJIT ctx j
    compd <- contextCompile ctx
    r <- f compd
    contextRelease ctx
    return r

getType :: JITTypeName -> JIT JITType
getType n = get >>= \c -> liftIO $ contextGetType c n

param :: Maybe JITLocation -> JITType -> ByteString -> JIT JITParam
param l t n = get >>= \c -> liftIO $ contextNewParam c l t n

function :: Maybe JITLocation -> JITFunctionKind -> JITType -> ByteString -> [JITParam] -> Bool -> JIT JITFunction
function l k t n p v = get >>= \c -> liftIO $ contextNewFunction c l k t n p v

block :: JITFunction -> Maybe ByteString -> JIT JITBlock
block f n = liftIO $ functionNewBlock f n

binaryOp :: Maybe JITLocation -> JITBinaryOp -> JITType -> JITRValue -> JITRValue -> JIT JITRValue
binaryOp l b t x y = get >>= \c -> liftIO $ contextNewBinaryOp c l b t x y

paramAsRValue :: JITParam -> JIT JITRValue
paramAsRValue p = liftIO $ _paramAsRValue p

blockEndWithReturn :: JITBlock -> Maybe JITLocation -> JITRValue -> JIT ()
blockEndWithReturn b l v = liftIO $ _blockEndWithReturn b l v
{-
withChildContext :: JIT a -> JIT a
withChildContext j = do
    ctx <- get
    cCtx <- liftIO $ newChildContext ctx
    r <- liftIO $ evalJIT cCtx j
    liftIO $ contextRelease cCtx
    return r
-}
