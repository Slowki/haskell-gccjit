-- Haskell version of https://gcc.gnu.org/onlinedocs/jit/intro/tutorial02.html
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Compiler.GCC.JIT

import Foreign.Ptr
import Foreign.C.Types

type FnType = CInt -> IO CInt
foreign import ccall "dynamic"
    mkFun :: FunPtr FnType -> FnType

createCode :: JIT ()
createCode = do
    setContextBoolOption JitDumpGeneratedCode True

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



    return ()

main :: IO ()
main = do
    res <- withContext createCode $ \r -> do
        fun <- resultGetCode r "loop_test"
        mkFun fun 10
    print res
