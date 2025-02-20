{-# LANGUAGE LambdaCase #-}
module Patterns where

newtype BankAccount = BankAccount
  { balance :: Double
  } deriving (Show)

createAccount :: Double -> BankAccount
createAccount initialBalance = BankAccount { balance = initialBalance }

deposit :: BankAccount -> Double -> BankAccount
deposit account amount = account { balance = balance account + amount }

withdraw :: BankAccount -> Double -> BankAccount
withdraw account amount = account { balance = balance account - amount }

displayBalance :: BankAccount -> IO ()
displayBalance account = putStrLn ("Current Balance: " ++ show (balance account))

performTransaction :: BankAccount -> IO ()
performTransaction account =
  putStrLn "Choose an option:" >>
  putStrLn "1. Deposit" >>
  putStrLn "2. Withdraw" >>
  putStrLn "3. Display Balance" >>
  putStrLn "4. Quit" >>
  getLine >>= \ case
    "1" -> putStrLn "Enter deposit amount:" >> readLn >>= \amount ->
            let newAccount = deposit account amount in
            displayBalance newAccount >> performTransaction newAccount

    "2" -> putStrLn "Enter withdrawal amount:" >> readLn >>= \amount ->
            let newAccount = withdraw account amount in
            (if balance newAccount < 0
                then putStrLn "Warning: Overdraft."
                else putStrLn "Withdrawal successful.") >>
            displayBalance newAccount >> performTransaction newAccount

    "3" -> displayBalance account >> performTransaction account

    "4" -> putStrLn "Goodbye!"

    _   -> putStrLn "Invalid choice. Please try again." >> performTransaction account

main :: IO ()
main =
  putStrLn "Welcome to the Haskell Bank!" >>
    putStrLn "Enter initial balance:" >>
    readLn >>= \initialBalance ->
      let account = createAccount initialBalance in
          performTransaction account
