module Server.Main where

import Prelude

import Effect (Effect)
import Server.Server as Server

main :: Effect Unit
main = do 
  server <- Server.main
  pure unit