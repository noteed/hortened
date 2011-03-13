{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}
module Main where

-- TODO recognize and sanitize URLs (so no unsafeByteStringValue).
-- TODO AJAXify the interface (via JSON).
-- TODO clean the String/ByteString mess.

import Paths_hortened (version)
import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit
import Database.HDBC.Sqlite3

import qualified Hortened.Httpd as Httpd
import qualified Hortened.Database as Db

versionString :: String
versionString =
  "Hortened " ++ showVersion version ++"\n\
  \The imaginatively named url-hortener server."

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes [httpd, addUser]
  &= summary versionString
  &= program "hortened"

data Cmd =
    Httpd
    { httpdPort :: Int
    , httpdHost :: String
    , httpdSqlite :: FilePath
    }
  | AddUser
    { addUserLogin :: String
    , addUserPassword :: String -- TODO prompt for the password
    , addUserSqlite :: FilePath
    }
  deriving (Data, Typeable)

httpd :: Cmd
httpd = Httpd
  { httpdPort = 8000
    &= explicit
    &= name "port"
    &= help "the port to listen on"
  , httpdHost = "localhost" -- TODO get it from the way the server is accessed
    &= explicit
    &= name "host"
    &= help "the hostname"
  , httpdSqlite = "hortened.sqlite" -- TODO better default
    &= help "SQLite file"
    &= explicit
    &= name "sqlite"
  } &= help "Run the Hortened HTTP server."

addUser :: Cmd
addUser = AddUser
  { addUserLogin = def -- TODO make it mandatory
    &= explicit
    &= name "user"
    &= name "u"
    &= help "the user to add"
  , addUserPassword = def -- TODO prompt for it instead
    &= explicit
    &= name "password"
    &= name "p"
    &= help "the user's password"
  , addUserSqlite = "hortened.sqlite" -- TODO better default
    &= help "SQLite file"
    &= explicit
    &= name "sqlite"
  } &= help "Add a user to the db."
    &= explicit
    &= name "add-user"

processCmd :: Cmd -> IO ()
processCmd Httpd{..} = Httpd.httpd httpdPort httpdHost httpdSqlite

processCmd AddUser{..} = do
  con <- connectSqlite3 addUserSqlite
  Db.createTables con
  _ <- Db.addUser con addUserLogin addUserPassword
  return ()

