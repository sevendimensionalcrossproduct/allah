module Calc where

multiply :: Int -> Int -> Int
multiply x y = x * y

factorial :: Int ->Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

cot :: Double ->Double
cot x = 1 / tan x

sec :: Double ->Double 
sec x = 1 / cos x

csc :: Double ->Double
csc x = 1 / sin x

acot :: Double ->Double
acot x
  | isInfinite x = pi / 2 * signum x
  | x == 0 = pi / 2
  | x < 0 = atan(1/x) + pi
  | x > 0 = atan(1/x)
  | otherwise = 0/0

asec :: Double ->Double
asec x = acos(1/x)

acsc :: Double ->Double
acsc x = asin(1/x)

coth :: Double ->Double
coth x = 1 / tanh x

sech :: Double ->Double
sech x = 1 / cosh x

csch :: Double ->Double
csch x = 1 / sinh x

acoth :: Double ->Double
acoth x = atanh (1/x)

asech :: Double ->Double
asech x = acosh (1/x)

acsch :: Double ->Double
acsch x = asinh (1/x) 

getFactorial :: IO ()
getFactorial =
  putStr "Input integer: " >>
  getLine >>= \input ->
  let n = read input :: Int in
  let result = factorial n in
  print result
