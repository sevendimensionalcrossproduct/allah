module Address where

import Prelude ((<>))
import Data.List (List)

type Address =
  { street :: String,
  city :: String,
  state :: String
  }

type  Entry =
  { firstName :: String, 
  lastName :: String,
  address :: Address
  }

type AddressBook = List Entry

showEntry :: Entry -> String


showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress address = address.street <> ", " <>
                      address.city <> ", " <>
                      address.state

