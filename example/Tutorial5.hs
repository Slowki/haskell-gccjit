-- Haskell version of https://gcc.gnu.org/onlinedocs/jit/intro/tutorial05.html
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Compiler.GCC.JIT

import Foreign.Ptr
import Foreign.C.Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Maybe (Maybe(..), isJust)
import Data.Array

import System.IO (readFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import Control.Monad (when)

data BFCompiler = BFCompiler {
    filename :: String,
    line :: Int,
    column :: Int,

    voidType :: JITType,
    intType :: JITType,
    byteType :: JITType,
    arrayType' :: JITType,

    funcGetchar :: JITFunction,
    funcPutchar :: JITFunction,

    func :: JITFunction,
    curblock :: JITBlock,

    intZero :: JITRValue,
    intOne :: JITRValue,
    byteZero :: JITRValue,
    byteOne :: JITRValue,
    dataCells :: JITLValue,
    idx :: JITLValue,

    numOpenParens :: Int,
    parenTest :: Array Int JITBlock,
    parenBody :: Array Int JITBlock,
    parenAfter :: Array Int JITBlock
}

type JIT' a = JITState BFCompiler a

maxOpenParens = 100

currentData :: JITLocation -> BFCompiler -> JIT' JITLValue
currentData l d = asRValue (dataCells d) >>= \d' -> asRValue (idx d) >>= \idx' -> arrayAccess (Just l) d' idx'

currentDataIsZero :: JITLocation -> BFCompiler -> JIT' JITRValue
currentDataIsZero l d = currentData l d >>= asRValue >>= \r -> comparison (Just l) JitEq r (byteZero d)

compileChar' :: JITLocation -> BFCompiler -> Char -> JIT' ()
compileChar' l d '>' = do
    addComment (curblock d) (Just l) "'>': idx += 1;"
    addAssignmentOp (curblock d) (Just l) (idx d) JitOpPlus (intOne d)
compileChar' l d '<' = do
    addComment (curblock d) (Just l)  "'<': idx -= 1;"
    addAssignmentOp (curblock d) (Just l) (idx d) JitOpMinus (intOne d)
compileChar' l d '+' = do
    addComment (curblock d) (Just l)  "'+': data[idx] += 1;"
    currentData l d >>= \lv -> addAssignmentOp (curblock d) (Just l) lv JitOpPlus (byteOne d)
compileChar' l d '-' = do
    addComment (curblock d) (Just l)  "'+': data[idx] += 1;"
    currentData l d >>= \lv -> addAssignmentOp (curblock d) (Just l) lv JitOpMinus (byteOne d)
compileChar' l d '.' = do
    arg <- currentData l d >>= asRValue >>= \lv -> cast (Just l) lv (intType d)
    call <- call (Just l) (funcPutchar d) [arg]
    addComment (curblock d) (Just l) "'.': putchar ((int)data[idx]);"
    addEval (curblock d) (Just l) call
compileChar' l d ',' = do
    call' <- call (Just l) (funcGetchar d) []
    addComment (curblock d) (Just l) "',': data[idx] = (unsigned char)getchar ();"
    currentData l d >>= \lv -> cast (Just l) call' (byteType d) >>= \c -> addAssignment (curblock d) (Just l) lv c
compileChar' l d '[' = do
    loopTest <- block (func d) Nothing
    onZero <- block (func d) Nothing
    onNonZero <- block (func d) Nothing

    when (numOpenParens d == maxOpenParens) $ error "too many open parens"

    endWithJump (curblock d) (Just l) loopTest
    addComment loopTest (Just l) "'['"

    currentDataIsZero l d >>= \iz -> endWithConditional loopTest (Just l) iz onZero onNonZero

    modify (\s -> s{
        parenTest = parenTest s // [(numOpenParens s, loopTest)],
        parenBody = parenBody s // [(numOpenParens s, onNonZero)],
        parenAfter = parenAfter s // [(numOpenParens s, onZero)],
        numOpenParens = numOpenParens s + 1,
        curblock = onNonZero
    })

compileChar' l d ']' = do
    addComment (curblock d) (Just l) "']'"
    when (numOpenParens d == 0) $ error "mismatching parens"

    modify (\s -> s { numOpenParens = numOpenParens s - 1 })
    d <- get
    endWithJump (curblock d) (Just l) (parenTest d ! numOpenParens d)
    modify (\s -> s { curblock = parenAfter s ! numOpenParens s })

compileChar' l d '\n' = modify (\s -> s{line = line s + 1, column = 0})
compileChar' _ _ _ = return ()

compileChar :: Char -> JIT' ()
compileChar c = do
    d <- get
    l <- location (pack $ filename d) (line d) (column d)
    liftIO $ putStrLn $ c : "-"
    compileChar' l d c

makeMain :: JIT' JITFunction
makeMain = do
    it <- getType JitInt
    argc <- param Nothing it "argc"
    cppt <- getType JitChar >>= getPointer >>= getPointer
    argv <- param Nothing cppt "argv"
    function Nothing JitFunctionExported it "main" [argc, argv] False

main :: IO Int
main = do
    (fname:oname:_) <- getArgs >>= \x -> if length x /= 2 then error "Usage: INPUT_FILE.bf OUTPUT_FILE" else return x
    genCode <- foldl (>>) (return ()) . map compileChar <$> readFile fname
    exitCode <- withContextAndState undefined $ do
        vt <- getType JitVoid
        it <- getType JitInt
        bt <- getType JitUnsignedChar
        at <- arrayType Nothing bt 30000

        gc <- function Nothing JitFunctionImported it "getchar" [] False

        paramC <- param Nothing it "c"
        pc <- function Nothing JitFunctionImported vt "putchar" [paramC] False

        main <- makeMain
        curblockinit <- block main (Just "initial")

        iz <- zero it
        io <- one it
        bz <- zero bt
        bo <- one bt
        dc <- global Nothing JitGlobalInternal at "data_cells"
        id <- local main Nothing it "idx"
        -- initial compiler state
        put BFCompiler{
            filename = fname,
            line = 1,
            column = 0,

            voidType = vt,
            intType = it,
            byteType = bt,
            arrayType' = at,

            funcGetchar = gc,
            funcPutchar = pc,

            func = main,
            curblock = curblockinit,

            intZero = iz,
            intOne = io,
            byteZero =  bz,
            byteOne = bo,
            dataCells = dc,
            idx = id,

            numOpenParens = 0,
            parenTest = listArray (0, maxOpenParens) [],
            parenBody = listArray (0, maxOpenParens) [],
            parenAfter = listArray (0, maxOpenParens) []
        }

        setIntOption JitOptimizationLevel 3
        setBoolOption JitDumpInitialGimple True
        setBoolOption JitDebuginfo True
        setBoolOption JitDumpEverything True

        setBoolOption JitKeepIntermediates True

        genCode
        s <- get
        endWithReturn (curblock s) Nothing (intZero s)

        compileToFile JitOutputExecutable (pack oname)
        e <- getFirstError
        if isJust e then return $ ExitFailure 1 else return ExitSuccess
    exitWith exitCode
