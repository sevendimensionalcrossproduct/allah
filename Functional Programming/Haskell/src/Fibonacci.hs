module Fibonacci where

import System.IO

main :: IO ()

main = do
        hSetBuffering stdout NoBuffering
        putStr "Enter integer: "
        input <-getLine
        let n = read input :: Int
        let result = fib n
        putStr "The result is: "
        print result

fib :: Int ->Int

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)
