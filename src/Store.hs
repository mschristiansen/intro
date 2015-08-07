-- This module creates an in-memory datastore using the `stm` library.
--
-- Some examples here:
-- http://book.realworldhaskell.org/read/software-transactional-memory.html

{-# LANGUAGE CPP #-}
module Store
  ( storeInit
  , storeInsert
  , storeLookup
  , storeList
  , storeSum
  , storeCount
  , storeAvg
  ) where

import Prelude hiding (lookup)
import Control.Concurrent.STM
import Data.IntMap (empty, insert, lookup, toAscList, foldl')
import Types
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

storeInit :: IO (TVar Collection)
storeInit = atomically $ newTVar empty


-- Basic Operations

insertPoint :: Point -> Collection -> Collection
insertPoint (Point x y) coll = insert x y coll

storeInsert :: Point -> TVar Collection -> IO ()
storeInsert p coll = atomically $ do
  c <- readTVar coll
  writeTVar coll $ insertPoint p c


storeAction :: (a -> b) -> TVar a -> IO b
storeAction action coll = atomically $ action <$> readTVar coll


lookupPointY :: Int -> Collection -> Maybe Int
lookupPointY = lookup

storeLookup :: Int -> TVar Collection -> IO (Maybe Int)
storeLookup = storeAction . lookupPointY


listPoints :: Collection -> [Point]
listPoints coll = map (uncurry Point) (toAscList coll)

storeList :: TVar Collection -> IO [Point]
storeList = storeAction listPoints



-- Data Aggregation

aggregatePoints :: Num a => (a -> Int -> a) -> Collection -> a
aggregatePoints op = foldl' op 0

sumPoints :: Collection -> Int
sumPoints = aggregatePoints (+)

storeSum :: TVar Collection -> IO Int
storeSum = storeAction sumPoints

countPoints :: Collection -> Int
countPoints = aggregatePoints (\acc _ -> acc + 1)

storeCount :: TVar Collection -> IO Int
storeCount  = storeAction countPoints

avgPoints :: Collection -> Int
avgPoints coll = sumPoints coll `div` countPoints coll

storeAvg :: TVar Collection -> IO Int
storeAvg = storeAction avgPoints
