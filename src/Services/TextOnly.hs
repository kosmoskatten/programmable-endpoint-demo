{-# LANGUAGE OverloadedStrings #-}
module Services.TextOnly
       ( routes
       ) where

import Control.Monad (replicateM_)
import Snap.Blaze (blaze)
import Simulation.Node.Service.Http
  ( HttpService
  , Routes (..)
  , putResponse
  )
import Simulation.Node.Service.Http.Server
  ( static
  , mkPrefixFunc
  , htmlResponse
  , loremIpsum
  )
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

routes :: Routes ()
routes = Routes [ ("1.html", page "Page 1" 1 ["2.html", "3.html", "4.html"])
                , ("2.html", page "Page 2" 2 ["1.html", "3.html", "4.html"])
                , ("3.html", page "Page 3" 4 ["1.html", "2.html", "4.html"])
                , ("4.html", page "Page 4" 8 ["1.html", "2.html", "3.html"])
                , ("static", static) ]

page :: H.Html -> Int -> [String] -> HttpService ()
page theTitle paragrafs links = do
  putResponse htmlResponse
  prefix <- mkPrefixFunc
  blaze $ do
    H.docType
    H.head $ do
      H.title theTitle
      H.script ! A.src (prefix "static/textonly.js") $ ""
      H.link ! A.href (prefix "static/textonly.css")
    H.body $ do
      replicateM_ paragrafs $ H.p loremIpsum
      mapM_ (\l -> H.a ! A.href (prefix l) $ (H.toHtml l)) links


