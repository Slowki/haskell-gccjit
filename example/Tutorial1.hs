-- Haskell version of https://gcc.gnu.org/onlinedocs/jit/intro/tutorial01.html
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Compiler.GCC.JIT

import Foreign.Ptr
import Foreign.C.Types

import Data.ByteString (useAsCString)

type FnType = Ptr CChar -> IO ()
foreign import ccall "dynamic"
  mkFun :: FunPtr FnType -> FnType

createCode :: JIT ()
createCode = do
    setContextBoolOption JitDumpGeneratedCode True

    voidType <- getType JitVoid
    constCharPtrType <- getType JitConstCharPtr
    paramName <- param Nothing constCharPtrType "name"
    func <- function Nothing JitFunctionExported voidType "greet" [paramName] False
    paramFormat <- param Nothing constCharPtrType "format"
    printfFunc <- getType JitInt >>= \itype -> function Nothing JitFunctionImported itype "printf" [paramFormat] True

    fmt <- stringLiteral "hello %s\n"
    rname <- paramAsRValue paramName

    blk <- block func Nothing
    newCall Nothing printfFunc [fmt, rname] >>= addEval blk Nothing
    endWithVoidReturn blk Nothing

    return ()

main :: IO ()
main = withContext createCode $ \r -> do
           fun <- resultGetCode r "greet"
           useAsCString "world" $ \c -> mkFun fun c
