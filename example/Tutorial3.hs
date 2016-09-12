-- Haskell version of https://gcc.gnu.org/onlinedocs/jit/intro/tutorial03.html
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Compiler.GCC.JIT

import Foreign.Ptr
import Foreign.C.Types

import Control.Monad.IO.Class (liftIO)

type FnType = CInt -> IO CInt
foreign import ccall "dynamic"
    mkFun :: FunPtr FnType -> FnType

createCode :: JIT ()
createCode = do
    theType <- getType JitInt
    let returnType = theType

    n <- param Nothing theType "n"
    func <- function Nothing JitFunctionExported returnType "loop_test" [n] False
    i <- local func Nothing theType "i"
    sum' <- local func Nothing theType "sum"

    bInitial <- block func $ Just "intial"
    bLoopCond <- block func $ Just "loop_cond"
    bLoopBody <- block func $ Just "loop_body"
    bAfterLoop <- block func $ Just "after_loop"

    zero theType >>= addAssignment bInitial Nothing sum'
    zero theType >>= addAssignment bInitial Nothing i
    endWithJump bInitial Nothing bLoopCond

    comp <- asRValue i >>= \ri -> asRValue n >>= \rn -> comparison Nothing JitGe ri rn
    endWithConditional bLoopCond Nothing comp bAfterLoop bLoopBody

    asRValue i >>= \ri -> binaryOp Nothing JitOpMult theType ri ri >>= addAssignmentOp bLoopBody Nothing sum' JitOpPlus

    one theType >>= addAssignmentOp bLoopBody Nothing i JitOpPlus

    endWithJump bLoopBody Nothing bLoopCond
    asRValue sum' >>= endWithReturn bAfterLoop Nothing

    return ()

main :: IO ()
main = do
    res <- withContext  $ do
        setBoolOption JitDumpGeneratedCode True
        createCode
        withResult $ \r -> do
            fun <- getCode r "loop_test"
            liftIO $ mkFun fun 10
    print res
