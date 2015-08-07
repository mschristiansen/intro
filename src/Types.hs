{-# LANGUAGE OverloadedStrings, CPP #-}
module Types where

import Data.IntMap (IntMap)
import Data.Aeson
import Control.Monad (mzero)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

data Point = Point
  { pointX :: Int
  , pointY :: Int
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Point where
  parseJSON (Object v) = Point <$> v .: "x" <*> v .: "y"
  parseJSON _          = mzero

instance ToJSON Point where
  toJSON (Point x y) = object ["x" .= x, "y" .= y]

type Collection = IntMap Int
