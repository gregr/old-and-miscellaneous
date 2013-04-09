module StateTransTest where

import Control.Monad.State
import Control.Monad.Identity

test1 = do a <- get
           modify (+ 1)
           b <- get
           return (a, b)

test2 = do a <- get
           modify (++ "1")
           b <- get
           return (a, b)

go1 = evalState test1 0
go2 = evalState test2 "0"

test3 = do modify (+ 1)
           lift $ modify (++ "1")
           a <- get
           b <- lift get
           return (a, b)

go3a = evalStateT test3 0
go3b = evalStateT (evalStateT test3 0) "0"
go3 = runIdentity $ evalStateT (evalStateT test3 0) "0"

test4 = do modify (+ 1)
           lift $ modify (++ "1")

go4a = evalStateT test4 0
go4 = runIdentity $ evalStateT (evalStateT test4 0) "0"

test4a = do modify (+ 1)
            test4b

test4b = do lift $ modify (++ "1")

go4b = runIdentity $ evalStateT (evalStateT test4a 0) "0"

test5 = do modify (+ 1)
           lift $ modify (++ "1")
           lift $ lift $ modify (\x -> x - 1)
           a <- get
           b <- lift get
           c <- lift $ lift get
           return (a, b, c)

go5 = runIdentity $ evalStateT (evalStateT (evalStateT test5 0) "0") 0
