module Compiler.GCC.JIT.Monad.Utilities where

import Compiler.GCC.JIT.Monad.Types (JIT, context)
import Compiler.GCC.JIT.Foreign.Types (JITContext)

import Control.Monad.IO.Class (liftIO)

inContext :: (JITContext -> IO a) -> JIT a
inContext f = context >>= liftIO . f

inContext1 :: (JITContext -> b -> IO c) -> b -> JIT c
inContext1 f x = context >>= \c' -> liftIO $ f c' x

inContext2 :: (JITContext -> b -> c -> IO d) -> b -> c -> JIT d
inContext2 f x y = context >>= \c' -> liftIO $ f c' x y

inContext3 :: (JITContext -> b -> c -> d -> IO e) -> b -> c -> d -> JIT e
inContext3 f x y z = context >>= \c' -> liftIO $ f c' x y z

inContext4 :: (JITContext -> b -> c -> d -> e -> IO f) -> b -> c -> d -> e -> JIT f
inContext4 f x y z z' = context >>= \c' -> liftIO $ f c' x y z z'

inContext5 :: (JITContext -> b -> c -> d -> e -> f -> IO g) -> b -> c -> d -> e -> f -> JIT g
inContext5 f x y z z' z'' = context >>= \c' -> liftIO $ f c' x y z z' z''

liftIO1 :: (a -> IO b) -> a -> JIT b
liftIO1 f x = liftIO $ f x

liftIO2 :: (a -> b -> IO c) -> a -> b -> JIT c
liftIO2 f x y = liftIO $ f x y

liftIO3 :: (a -> b -> c -> IO d) -> a -> b -> c -> JIT d
liftIO3 f x y z = liftIO $ f x y z

liftIO4 :: (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> JIT e
liftIO4 f x y z z' = liftIO $ f x y z z'

liftIO5 :: (a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> JIT f
liftIO5 f x y z z' z'' = liftIO $ f x y z z' z''
