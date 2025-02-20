module Calibrate where

import Data.Char (isDigit)

filterDigits :: String -> String 
filterDigits = filter isDigit


main :: IO()
main =
  putStrLn "Input: " >>
  getLine >>= \ input ->
  putStrLn ("Result: " ++ show (read (filterDigits input) :: Int))
