module Address where

import Prelude

data Address = Address
  {street :: String
  , city :: String  -- These are functions btw
  , state :: String 
  }

data Entry = Entry
  {firstName :: String,
  lastName :: String,
  address :: Address
  }


showEntry :: Entry -> String
showEntry entry =
  lastName entry <> ", " <> 
  firstName entry <> ": " <>
  showAddress (address entry)


showAddress :: Address -> String
showAddress someAddress = 
  street someAddress ++ ", " ++ 
  city someAddress ++ ", " ++ 
  state someAddress ++ "."

{--IO--}
addressFun :: IO ()
addressFun =

  putStr "Input street: " >>
  getLine >>= \ streetInput -> 
  putStr "Input city: " >>
  getLine >>= \ cityInput -> 
  putStr "Input state: " >>
  getLine >>= \ stateInput -> 

  let addressEntry = Address 
        {
        street = streetInput,
        city = cityInput,
        state = stateInput
        }
  
   in putStrLn "\nAddress Entered \n" >>
  
  putStr "Input first name: " >>
  getLine >>= \ fnInput ->
  putStr "Input last name: " >>
  getLine >>= \ lnInput ->
  
  let nameNaddress = Entry 
        {
        firstName = fnInput,
        lastName=lnInput,
        address=addressEntry
        }
  in putStrLn (showEntry nameNaddress)
