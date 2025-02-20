module Dual where

import Calc
import GHC.Generics (D)

data DualNumber = DualNumber Double Double

{--Instantiation of Dual Numbers--}
instance Show DualNumber where
    show (DualNumber x y)
        | x == 0 && y == 0.0 = "0" 

        | x /= 0 && y == 1 = show x ++ " + ε"
        | x /= 0 && y == -1 = show x ++ " - ε"
        | x == 0 && y == 1 = "ε"
        | x == 0 && y == -1 = "-ε"
         

        | x /= 0 && y > 0 = show x ++ " + " ++ show y ++ "ε" 
        | x /= 0 && y < 0 = show x ++ " - " ++ show (abs y) ++ "ε"

        | x == 0 && y /= 0 = show y ++ "ε" 
        | x /= 0 && y == 0 = show x 
        
        | otherwise = "0"
        

{--Getter Functions--}
getReal :: DualNumber -> Double
getReal (DualNumber x _) = x 

getEpsilon :: DualNumber -> Double
getEpsilon (DualNumber _ y) = y


{--Basic Operations--}
add :: DualNumber ->DualNumber -> DualNumber
add (DualNumber x1 y1) (DualNumber x2 y2) =  DualNumber (x1 + x2) (y1 + y2)

substr :: DualNumber -> DualNumber -> DualNumber
substr (DualNumber x1 y1) (DualNumber x2 y2) = DualNumber (x1 - x2) (y1 -y2)

mult :: DualNumber -> DualNumber -> DualNumber
mult (DualNumber x1 y1) (DualNumber x2 y2) = DualNumber (x1 * x2) ((x1 * y2) + (y1 * x2))

div :: DualNumber -> DualNumber -> DualNumber
div (DualNumber x1 y1) (DualNumber x2 y2) = DualNumber (x1 / x2) (((y1 * x2) - (x1 * y2)) / (x2 * x2))


{--Elementary Operations--}

dnSin :: DualNumber -> DualNumber 
dnSin z = DualNumber (sin(getReal z)) (cos (getReal z) * getEpsilon z)

dnCos :: DualNumber -> DualNumber
dnCos z = DualNumber (cos(getReal z)) (-sin(getReal z) * getEpsilon z)

dnTan :: DualNumber -> DualNumber
dnTan z = DualNumber (tan(getReal z)) (sec(getReal z)**2 * getEpsilon z)

dnCot :: DualNumber -> DualNumber
dnCot z = DualNumber (cot (getReal z)) (-csc(getReal z)**2 * getEpsilon z)

dnSec :: DualNumber -> DualNumber
dnSec z = DualNumber (sec(getReal z)) (sec(getReal z) * tan(getReal z) * getEpsilon z)

dnCsc :: DualNumber -> DualNumber
dnCsc z = DualNumber (csc(getReal z)) (-csc(getReal z) * cot(getReal z) * getEpsilon z)


dnArcsin :: DualNumber -> DualNumber
dnArcsin z = DualNumber (asin(getReal z)) (getEpsilon z / sqrt(1 - getReal z**2))

dnArccos :: DualNumber -> DualNumber
dnArccos z = DualNumber (acos(getReal z)) (-getEpsilon z / sqrt(1 -getReal z**2))

dnArctan :: DualNumber -> DualNumber
dnArctan z = DualNumber (atan(getReal z)) (getEpsilon z / (getReal z^^2 + 1))

dnArccot :: DualNumber -> DualNumber
dnArccot z = DualNumber (acot (getReal z)) (-getEpsilon z / (getReal z^^2 + 1))

dnArcsec :: DualNumber -> DualNumber
dnArcsec z = DualNumber (asec (getReal z)) (getEpsilon z / (abs (getReal z) * (sqrt(getReal z**2 - 1))))

dnArccsc :: DualNumber -> DualNumber
dnArccsc z = DualNumber (acsc (getReal z)) (-getEpsilon z / (abs (getReal z) * (sqrt(getReal z**2 - 1))))


dnSinh :: DualNumber -> DualNumber
dnSinh z = DualNumber (sinh (getrReal z)) (cosh (getReal z) * getEpsilon z)

dnCosh :: DualNumber -> DualNumber
dnCosh z = DualNumber (cosh(getReal z)) (sinh (getReal z) getEpsilon z)

dnTanh :: DualNumber -> DualNumber
dnTanh = DualNumber (tanh(getReal z)) (sech z**2 * getEpsilon z)

dnCoth :: DualNumber -> DualNumber
dnCoth z = DualNumber (coth(getReal z)) ()

dnCsch :: DualNumber -> DualNumber
dnCsch z = DualNumber (csch(getReal z)) ()


dnArsinh :: DualNumber -> DualNumber
dnArsinh z = DualNumber (asinh(getReal z)) ()

dnArcosh :: DualNumber -> DualNumber
dnArcosh z = DualNumber (acosh(getReal z)) ()

dnArtanh :: DualNumber -> DualNumber
dnArtanh z = DualNumber (atanh(getReal z)) ()

dnArcoth :: DualNumber -> DualNumber
dnArcoth z = DualNumber (acoth(getReal z)) ()

dnArsech :: DualNumber -> DualNumber
dnArsech z = DualNumber (asech(getReal z)) ()

dnArcsch :: DualNumber -> DualNumber
dnArcsch z = DualNumber (acsch(getReal z)) ()


dnLn :: DualNumber -> DualNumber
dnLn z = DualNumber (log(getReal z)) ()

dnLog :: DualNumber -> DualNumber
dnLog = DualNumber () ()

dnExp :: DualNumber -> DualNumber
dnExp = DualNumber () ()

dnReciprocal:: DualNumber -> DualNumber
dnReciprocal = DualNumber () ()

dnSqrt:: DualNumber -> DualNumber

dnSqrt = DualNumber () ()

dnCbrt:: DualNumber -> DualNumber
dnCbrt = DualNumber () (

dnReciprocal:: DualNumber -> DualNumber

dnReciprocal = DualNumber () ()
