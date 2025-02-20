module Str where

name :: IO ()

name =
  putStr "Input first name: " >>
  getLine >>= \ firstn ->
  putStr "Input last name: " >>
  getLine >>= \ lastn ->
  let full = firstn ++ " " ++ lastn
  in putStr ("Name: " ++ full ++ "\n")
