module Hello where

import Effect (Effect)
import Effect.Console (log)
import Data.Unit (Unit)

sayHi :: Effect Unit
sayHi = log "Hi"
