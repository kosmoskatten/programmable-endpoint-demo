{-# LANGUAGE OverloadedStrings #-}
module Services.TextAndImages
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
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

routes :: Routes ()
routes = Routes [ ( "1.html", page "Page 1" 4 ["2.html", "3.html", "4.html"]
                                              allImages )
                , ( "2.html", page "Page 2" 8 ["1.html", "3.html", "4.html"]
                                              allImages )
                , ( "3.html", page "Page 3" 7 ["1.html", "2.html", "4.html"]
                                              allImages )
                , ( "4.html", page "Page 4" 13 ["1.html", "2.html", "3.html"]
                                              allImages )
                , ( "static", static) ]

page :: H.Html -> Int -> [String] -> [String] -> HttpService ()
page theTitle paragrafs links images = do
  putResponse htmlResponse
  prefix <- mkPrefixFunc
  blaze $ do
    H.docType
    H.head $ do
      H.title theTitle
      H.script ! A.src (prefix "static/textandimages.js") $ ""
      H.link ! A.href (prefix "static/textandimages.css")
    H.body $ do
      replicateM_ paragrafs $ H.p loremIpsum
      mapM_ (\l -> H.a ! A.href (prefix l) $ (H.toHtml l)) links
      mapM_ (\img -> H.img ! A.src (prefix img)) images

allImages :: [String]
allImages = [ "static/image1.jpg", "static/image2.jpg", "static/image3.jpg"
            , "static/image4.jpg", "static/image5.jpg" ]

