{-# LANGUAGE OverloadedStrings #-}
module Services.TextOnly
       ( routes
       ) where
       
import Simulation.Node.Service.Http (HttpService, Routes (..), writeBS)       

routes :: Routes ()
routes = Routes [ ("/", frontPage) ]

frontPage :: HttpService ()
frontPage = writeBS "frontPage"