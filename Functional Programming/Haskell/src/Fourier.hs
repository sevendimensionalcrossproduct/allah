module Fourier where

import Index (stringToIndices)
import Data.List (tails)

newtype Polynomial = Polynomial [Double]

instance Show Polynomial where
    show (Polynomial xs) = showPolynomial (zip [0..] xs) where

        showPolynomial :: [(Integer, Double)] -> String
        showPolynomial [] = ""
        showPolynomial ((i, a):rest) = showStream i a ++ showPolynomial rest

        showStream :: Integer -> Double -> String
        showStream i a
            | a == 0 = ""
            | otherwise = getSign i a ++ formatCoefficient i a ++ formatX i a ++ formatIndex i

        getSign :: Integer -> Double -> String
        getSign i x = case i of
                0 -> if signum x == -1 then "-"  else ""
                _ -> if signum x == -1 then " - " else if signum x == 1 then " + " else ""

        formatCoefficient :: Integer ->Double -> String
        formatCoefficient i a
            | abs a == 1 && i > 1 = ""
            | isIntegral a = show (abs (round a) :: Int)
            | otherwise = show (abs a)

        formatX :: Integer ->Double -> String
        formatX i a
            | a == 0 || i == 0 = ""
            | otherwise = "x"

        isIntegral :: Double -> Bool
        isIntegral x = snd (properFraction x :: (Integer, Double)) == 0

        formatIndex :: Integer -> String
        formatIndex i
            | i == 0 || i == 1 = ""
            | otherwise = stringToIndices (show i)


add :: Polynomial -> Polynomial -> Polynomial
add (Polynomial xs) (Polynomial ys) = Polynomial [x + y |(x, y) <- zip xs' ys'] where
    maxLength = max (length xs) (length ys)
    xs' = xs ++ replicate (maxLength - length xs) 0
    ys' = ys ++ replicate (maxLength - length ys) 0


convolve :: Polynomial -> Polynomial -> Polynomial
convolve (Polynomial xs) (Polynomial ys) = Polynomial [sum  (zipWith (*) (reverse xs) i) | i <- tails pady] where
    pady = replicate (length xs - 1) 0 ++ ys
