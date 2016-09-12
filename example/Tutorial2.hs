-- Haskell version of https://gcc.gnu.org/onlinedocs/jit/intro/tutorial02.html
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
    intType <- getType JitInt
    paramI <- param Nothing intType "i"
    func <- function Nothing JitFunctionExported intType "square" [paramI] False
    block <- block func Nothing
    expr <- asRValue paramI >>= \rv -> binaryOp Nothing JitOpMult intType rv rv
    endWithReturn block Nothing expr
    return ()

main :: IO ()
main = do
    res <- withContext $ do
        setBoolOption JitDumpGeneratedCode True
        createCode
        withResult $ \r -> do
            fun <- getCode r "square"
            liftIO $ mkFun fun 5
    print res
