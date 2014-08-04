{-# LANGUAGE OverloadedStrings #-}
module Services.TextOnly
       ( routes
       ) where

import Control.Applicative ((<$>))
import Snap.Blaze (blaze)
import Snap.Util.FileServe (serveDirectory)
import Simulation.Node.Service.Http
  ( HttpService
  , Routes (..)
  , Response
  , basePrefix
  , selfStore
  , putResponse
  , setResponseCode
  , setContentType
  , emptyResponse
  , writeBS
  )
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

routes :: Routes ()
routes = Routes [ ("1.html", page "Page 1" ["2.html", "3.html", "4.html"])
                , ("2.html", page "Page 2" ["1.html", "3.html", "4.html"])
                , ("3.html", page "Page 3" ["1.html", "2.html", "4.html"])
                , ("4.html", page "Page 4" ["1.html", "2.html", "3.html"])
                , ("static", static) ]

page :: H.Html -> [String] -> HttpService ()
page theTitle links = do
  putResponse htmlResponse
  prefix <- mkPrefix
  blaze $ do
    H.docType
    H.head $ do
      H.title theTitle
      H.script ! A.src (prefix "static/textonly.js") $ ""
      H.link ! A.href (prefix "static/textonly.css")
    H.body $ do
      H.p loremIpsum
      mapM_ (\l -> H.a ! A.href (prefix l) $ (H.toHtml l)) links

static :: HttpService ()
static = serveDirectory =<< selfStore

htmlResponse :: Response
htmlResponse =
  setResponseCode 200 $
  setContentType "text/html" emptyResponse

mkPrefix :: HttpService (String -> H.AttributeValue)
mkPrefix = appendTo <$> basePrefix
  where
    appendTo :: String -> String -> H.AttributeValue
    appendTo serviceBase resource = H.toValue $ serviceBase ++ resource

loremIpsum :: H.Html
loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit.\
    \Donec ac ligula at leo consequat volutpat.\
    \Duis egestas augue eu lorem aliquam lacinia.\
    \Curabitur condimentum eu eros ut tincidunt. Duis vitae turpis erat.\
    \Ut aliquam massa nec scelerisque congue.\
    \Nulla rhoncus odio est, vulputate fringilla lectus sodales sit amet.\
    \Donec nec pellentesque dui. Fusce congue quam eu posuere suscipit.\
    \Duis diam dolor, semper auctor luctus quis, condimentum eu est.\
    \Mauris lobortis aliquam velit, ac porta quam rhoncus id.\
    \Nullam at mattis erat.\
    \Aenean blandit augue tortor, quis euismod elit sodales in.\
    \Phasellus et condimentum libero, eu feugiat mauris.\
    \Etiam condimentum libero quis magna pellentesque ornare.\
    \Cras nunc libero, fermentum in eleifend tempor, blandit eu arcu."
