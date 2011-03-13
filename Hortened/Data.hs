{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hortened.Data where

-- TODO check column sizes for passwords, hashes, ...
-- link_id should not be auto-increment (to reuse ids
-- when the corresponding link is removed from db).

import Database.HDBC.Sqlite3

import Data.IORef
import qualified Data.ByteString.Char8 as B

data Link = Link -- TODO use this type to interact with the DB
  { linkShort :: Int
  -- ^ The shortened version of the URL.
  , linkLong :: B.ByteString
  -- ^ The original URL this link points to.
  -- , linkCreated :: UTCTime
  -- ^ When the link was created.
  -- , linkClicks :: Int
  -- ^ How many times the short url was queried.
  -- TODO - Number of click per last day, week, month, year, all time.
  --      - Referrals, user countries, ...
  }

data H = H
  { hHost :: String
  , hPort :: Int
  , hDb :: Connection
  , hKey :: B.ByteString
  , hSessions :: IORef [(Int, Integer)] -- mapping cookie / user id
  , hUser :: Maybe Integer
  }

