module Parentheses where

parentheses :: Char ->Int
parentheses a
  | a == ')' = -1
  | a == '(' = 1
  | otherwise = 0

parenthesesString :: String -> Int
parenthesesString = sum  . map parentheses 

main :: IO()
main =
 putStrLn "Input: " >>
 getLine >>= \ input ->
 putStrLn ("Result: " <> show (parenthesesString input))

