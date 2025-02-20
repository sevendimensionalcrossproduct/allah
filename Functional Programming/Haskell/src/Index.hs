module Index where

superindex :: Char -> Char 
superindex input =
   case input of
    '0' -> '⁰'
    '1' -> '¹'
    '2' -> '²'
    '3' -> '³'
    '4' -> '⁴'
    '5' -> '⁵'
    '6' -> '⁶'
    '7' -> '⁷'
    '8' -> '⁸'
    '9' -> '⁹'
    '.' -> '·'
    _ -> ' '

stringToIndices :: String -> String
stringToIndices  = map superindex 

getIndex :: IO ()
getIndex =
  putStr "Enter integer: " >>
  getLine >>= \ input ->putStrLn (stringToIndices input)
