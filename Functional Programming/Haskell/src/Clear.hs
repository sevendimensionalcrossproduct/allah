module Clear where

import System.Console.ANSI

clear :: IO ()
clear = do
  System.Console.ANSI.clearScreen
  setCursorPosition 0 0
