-- Haskell version of https://gcc.gnu.org/onlinedocs/jit/intro/tutorial02.html
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Compiler.GCC.JIT

import Foreign.Ptr
import Foreign.C.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe

type FnType = CInt -> IO CInt
foreign import ccall "dynamic"
  mkFun :: FunPtr FnType -> FnType

createCode :: JIT ()
createCode = do
    intType <- getType JitInt
    paramI <- param Nothing intType "i"
    func <- function Nothing JitFunctionExported intType "square" [paramI] False
    block <- block func Nothing
    expr <- paramAsRValue paramI >>= \rv -> binaryOp Nothing JitOpMult intType rv rv
    blockEndWithReturn block Nothing expr
    return ()

main :: IO ()
main = do
    res <- withContext createCode $ \r -> do
        fun <- resultGetCode r "square"
        mkFun fun 5
    print res
