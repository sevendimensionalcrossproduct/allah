module Person where

import Prelude

import Data.List (drop)
import Effect (Effect)
import Effect.Console (log)

data Person = Person { name :: String, age :: Int}

showPerson :: Person -> String

showPerson (Person o) = o.name <> " , aged " <> show o.age 

examplePerson :: Person
examplePerson = Person { name: "Bonnie", age: 26}

main :: Effect Unit
main = do 
  log "Enter name:"
