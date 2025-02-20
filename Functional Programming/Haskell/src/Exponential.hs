module Exponential where

import Index (stringToIndices)


square :: Int -> Int
square x = x * x 

exponentiate :: Double ->Double -> Double
exponentiate _ 0 = 1.0
exponentiate base expn
  | expn > 0 = base ** expn
  | otherwise = 1.0 / (base ** (-expn))

main :: IO ()
main =
  putStr "Base: " >>
  getLine >>= \ base -> 
  let baseNum = read base :: Double in
 
  putStr "Exponent: " >>
  getLine >>= \ expn ->
  let expnNum = read expn :: Double in

  putStr (base <>  stringToIndices expn <> " = ") >> 
  print (exponentiate baseNum expnNum)

