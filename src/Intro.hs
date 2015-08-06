{-# LANGUAGE OverloadedStrings #-}
module Intro where

import Control.Monad.IO.Class (liftIO)
import Pages
import Store
import System.Random
import Types
import Web.Scotty


startServer :: IO ()
startServer = do
  coll <- storeInit
  scotty 3000 $ do

    -- HTML Interface
    get "/" $ html home
    get "/store/list" $ do
      points <- liftIO $ storeList coll
      html $ showColl points
    get "/store/insert/:x/:y" $ do
      x <- param "x"
      y <- param "y"
      let point = Point x y
      liftIO $ storeInsert point coll
      html $ confirmInsert point

    -- Insert `:count` random numbers into the store.
    get "/store/insert/random/:count" $ do
      i <- param "count"
      g <- liftIO $ getStdGen
      let points = take i $ zipWith Point [1..] (randomRs (1,100) g)
      liftIO $ mapM_ (flip storeInsert coll) points
      html $ showColl points

    -- Aggregation functions
    get "/store/sum" $ (liftIO $ storeSum coll) >>= (html . showResult)
    get "/store/count" $ (liftIO $ storeCount coll) >>= (html . showResult)
    get "/store/average" $ (liftIO $ storeAvg coll) >>= (html . showResult)

    -- JSON API
    get "/api/list" $ (liftIO $ storeList coll) >>= json
    put "/api/insert" $ do
      point <- jsonData
      liftIO $ storeInsert point coll
      json ("ok" :: String)
    notFound $ html page404
