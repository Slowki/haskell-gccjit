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

    intType <- getType JitInt
    paramI <- param Nothing intType "i"
    func <- function Nothing JitFunctionExported intType "square" [paramI] False
    block <- block func Nothing
    expr <- paramAsRValue paramI >>= \rv -> binaryOp Nothing JitOpMult intType rv rv
    endWithReturn block Nothing expr
    return ()

main :: IO ()
main = do
    res <- withContext createCode $ \r -> do
        fun <- resultGetCode r "square"
        mkFun fun 5
    print res
