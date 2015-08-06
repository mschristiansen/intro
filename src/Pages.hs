-- Some html pages that can be displayed by Scotty.
--
-- The `blaze-html` library used for html markup is described here:
-- http://hackage.haskell.org/package/blaze-html
--

{-# LANGUAGE OverloadedStrings #-}
module Pages where

import Data.Text.Lazy
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types
import Control.Monad


wrapHtml :: H.Html -> H.Html -> Text
wrapHtml ptitle pbody = renderHtml $ docTypeHtml $ do
  H.head $ H.title ptitle
  H.body $ pbody

home :: Text
home = wrapHtml "Introduction to Haskell" $ do
  h1 "Introduction to Haskell"
  p "This is a small server for in-memory storage of data points."
  hr
  a "Show all points in the store." ! href "/store/list"
  p "Insert a point by using `/store/insert/x/y` where `x` and `y` are numbers."
  a "Example" ! href "/store/insert/1/3"
  a "Insert 100 random points" ! href "/store/insert/random/100"
  hr
  p "Test the JSON API using `curl`."
  p "Get JSON list."
  p "`curl -X GET http://localhost:3000/api/list`"
  p "Insert JSON data point."
  p "`curl -X PUT -v -H 'Content-Type: application/json' http://localhost:3000/api/insert --data '{\"x\": 3, \"y\": 10}'`"

showColl :: [Point] -> Text
showColl points = wrapHtml "Collection" $ do
  h1 "Collection List"
  table $ do
    thead $ do
      tr $ do
        th "X"
        th "Y"
    tbody $ forM_ points $ \point -> do
      tr $ do
        td $ toHtml $ pointX point
        td $ toHtml $ pointY point

confirmInsert :: Point -> Text
confirmInsert point = wrapHtml "Confirmation" $
  p $ toHtml $ show point ++ " inserted."

page404 :: Text
page404 = wrapHtml "Not Found" $
  p "Sorry, the page you requested could not be found."

showResult :: (ToMarkup a) => a -> Text
showResult result = wrapHtml "Collection" $ do
  h1 "Collection List"
  p $ toHtml result
