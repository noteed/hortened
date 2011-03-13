module Hortened.Database where

-- TODO check column sizes for passwords, hashes, ...
-- link_id should not be auto-increment (to reuse ids
-- when the corresponding link is removed from db).

import Control.Monad
import Control.Monad.Reader
import Data.Maybe (isJust)

import Database.HDBC
import System.Random
import Crypto.PBKDF2

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as W
import Data.Time.Clock.POSIX

import Hortened.Base62 (decode)

-- Create the tables if they don't already exist.
createTables :: IConnection c => c -> IO ()
createTables con = do
  tables <- getTables con
  when ("users" `notElem` tables) $ do
    run con
      "CREATE TABLE users (\
      \user_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
      \user_login CHAR(64) NOT NULL UNIQUE,\
      \user_password CHAR(64) NOT NULL,\
      \user_salt CHAR(64) NOT NULL)" -- TODO change to bytea type
      []
    return ()
  when ("links" `notElem` tables) $ do
    run con
      "CREATE TABLE links (\
      \link_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
      \link_long TEXT NOT NULL,\
      \link_user integer REFERENCES users(user_id),\
      \link_created integer NOT NULL,\
      \link_clicks integer NOT NULL)" -- TODO replace by a table to give some context (date, ...)
      []
    return ()
  commit con

-- TODO hash the password
addUser :: IConnection c => c -> String -> String -> IO Integer
addUser con login password = handleSql errorHandler $ do
  g <- liftIO newStdGen
  let salt = B.pack . show . fst $ next g
  run con "INSERT INTO users \
    \(user_login, user_password, user_salt) VALUES (?,?,?)"
    [toSql login, toSql $ hashPassword (B.pack password) salt, toSql salt]
  r <- quickQuery' con "SELECT last_insert_rowid()"
    []
  case r of
    [[x]] -> do
      commit con
      return $ fromSql x
    y -> fail $ "addUser: unexpected result: " ++ show y
  where
  errorHandler e = fail $
    "Error adding user; does the login already exist?\n" ++ show e

getUser :: IConnection c => c -> String -> String -> IO (Maybe Integer)
getUser con login password = handleSql errorHandler $ do
  r <- quickQuery' con
    "SELECT user_id, user_password, user_salt FROM users WHERE user_login=?"
    [toSql login]
  case r of
    [[i, p, s]] | fromSql p == hashPassword (B.pack password) (fromSql s)
      -> return . Just $ fromSql i
    _ -> return Nothing
  where
  errorHandler e = fail $
    "Error getting user.\n" ++ show e

addLink :: IConnection c => c -> String -> Maybe Integer -> IO Integer
addLink con long user = handleSql errorHandler $ do
  t <- getPOSIXTime
  run con "INSERT INTO links (link_long, link_user, link_clicks, link_created) VALUES (?,?,?,?)"
    [toSql long, toSql user, toSql (0::Integer), toSql t]
  r <- quickQuery' con "SELECT last_insert_rowid()"
    []
  case r of
    [[x]] -> do
      commit con
      return $ fromSql x
    y -> fail $ "addLink: unexpected result: " ++ show y
  where
  errorHandler e = fail $
    "Error adding link\n" ++ show e

-- When the boolean is True, this will increment the link_clicks
-- column.
getLink :: IConnection c => c -> String -> Bool -> IO (Maybe String)
getLink con short inc = handleSql errorHandler $ do
  let short' = decode short + 1
  r <- quickQuery' con
    "SELECT link_long FROM links WHERE link_id=?"
    [toSql short']
  case r of
    [[x]] -> do
      when inc $ run con
        "UPDATE links SET link_clicks = link_clicks + 1 WHERE link_id=?"
        [toSql short'] >> return ()
      return . Just $ fromSql x
    y -> return Nothing
  where
  errorHandler e = fail $
    "Error getting link\n" ++ show e

getLastLinks :: IConnection c => c -> IO [(Int, String, Int, POSIXTime, Bool)]
getLastLinks con = do
  r <- quickQuery' con
    "SELECT link_id, link_long, link_created, link_clicks, link_user FROM links ORDER BY link_id DESC LIMIT 10"
    []
  let f [i, l, t, c, u] = (fromSql i - 1, fromSql l, fromSql c, fromSql t, isJust (fromSql u :: Maybe Integer))
  return $ map f r

hashPassword :: B.ByteString -> B.ByteString -> B.ByteString
hashPassword pwd salt =
  let HashedPass x = pbkdf2 (Password $ W.unpack pwd) (Salt $ W.unpack salt)
  in W.pack x
