{-# LANGUAGE OverloadedStrings #-}

module Hortened.Httpd where

import System.FilePath ((</>))

import Control.Applicative ((<$>), (<|>))
import Control.Monad.Reader (asks)
import Control.Monad (when)
import Network.URI -- TODO use it to parse the URIs

import Snap.Types
import Snap.Http.Server (httpServe, defaultConfig)
import Snap.Http.Server.Config (addListen, setVerbose, ConfigListen(..))

import qualified Data.ByteString.Char8 as B
  (ByteString, unpack, append, pack, readInt, null)
import Text.Blaze (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderHtml)

import Data.IORef
import Control.Monad.IO.Class (liftIO)
import System.Random
import Data.Time.Clock.POSIX
import System.Locale
import Data.Time.Format

import Web.ClientSession
import Database.HDBC.Sqlite3 (connectSqlite3)

import Hortened.Data
import Hortened.Database
import Hortened.Snap
import Hortened.Base62

myCookieName :: B.ByteString
myCookieName = "hortened"

httpd :: Int -> String -> FilePath -> IO ()
httpd port host dbFile = do
  con <- connectSqlite3 dbFile
  createTables con
  key <- getDefaultKey
  sessions <- newIORef []
  let h = H  host port con key sessions
  httpServe (addListen (ListenHttp "127.0.0.1" port) $
             addListen (ListenHttps "127.0.0.1" (succ port)
             "/home/thu/projects/hy.io/certificates/server.crt"
             "/home/thu/projects/hy.io/certificates/server.key") $
             setVerbose False defaultConfig) $ do
    ss <- liftIO $ readIORef sessions
    msid <- myCookie key
    let mu = msid >>= flip lookup ss
    flip runHortened (h mu) $
      ifTop home
      <|> route routes

headline :: Hortened Html
headline = do
  secure <- isSecure
  logged <- isLogged
  return $ do
    "Hortened"
    when secure $
      if logged then H.a ! A.href "/a/logout" $ "logout"
                else H.a ! A.href "/a/login" $ "login"

home :: Hortened ()
home = do
  h <- headline
  blaze $ do
    h
    H.form ! A.method "POST" ! A.action "/a/shorten" $ do -- TODO enctype?
      H.input ! A.type_ "text"
              ! A.name (H.stringValue "shorten-input")
              ! A.id (H.stringValue "shorten-input")
              ! A.value (H.stringValue "")
      H.input ! A.type_ "submit" ! A.value "Shorten"

routes :: [(B.ByteString, Hortened ())]
routes =
  [ ("/ ", showLinks)
  , ("/a/shorten", actionShorten)
  , ("/a/login", loginHandler)
  , ("/a/logout", logoutHandler)
  --, ("/a/cookies", cookiesPage) -- TODO only for debugging
  , ("/:short", short)
  ]

actionShorten :: Hortened ()
actionShorten = do
  method' <- rqMethod <$> getRequest
  case method' of
    POST -> do
      ml <- getParam "shorten-input"
      case ml of
        Nothing  -> blaze $ H.p "malformed POST request." -- TODO
        Just l -> do
          con <- asks hDb
          mu <- asks hUser
          i <- liftIO $ addLink con (B.unpack l) mu
          blaze $ H.unsafeByteString . encodeBS . fromIntegral $ i - 1
    _ -> error "TODO"

showLinks :: Hortened ()
showLinks = do
  con <- asks hDb
  ls <- liftIO $ getLastLinks con
  h <- headline
  blaze $ do
    h
    H.table $ mapM_ (showLink "hy.io") ls

showLink :: FilePath -> (Int, String, Int, POSIXTime, Bool) -> Html
showLink h (s, l, n, t, u) = H.tr $ do
  H.td $ H.a ! A.href (H.unsafeByteStringValue $ "http://" `B.append` B.pack l) $
    H.unsafeByteString ("http://" `B.append` B.pack l)
  H.td $ H.string $ "http://" ++ h </> encode s
  H.td $ H.string $ formatTime defaultTimeLocale "%Y-%m-%d" (posixSecondsToUTCTime t)
  H.td . H.string $ show n
  H.td . H.string $ if u then "" else "(temporary)"

loginHandler :: Hortened ()
loginHandler = do
  secure <- isSecure
  if not secure
    then
      blaze "Login is possible only through HTTPS."
    else do
      method' <- rqMethod <$> getRequest
      case method' of
        GET -> blaze $
          H.form ! A.method "POST" ! A.action "/a/login" $ do -- TODO enctype?
            H.label ! A.for "login-username" $ "Username:"
            H.input ! A.type_ "text"
                    ! A.name (H.stringValue "login-username")
                    ! A.id (H.stringValue "login-username")
                    ! A.value (H.stringValue "")
            H.label ! A.for "login-password" $ "Password:"
            H.input ! A.type_ "text"
                    ! A.name (H.stringValue "login-password")
                    ! A.id (H.stringValue "login-password")
                    ! A.value (H.stringValue "")
            H.input ! A.type_ "submit" ! A.value "Login"
        POST -> do
          ml <- getParam "login-username"
          mp <- getParam "login-password"
          case (ml, mp) of
            (Just l, Just p) -> do
              con <- asks hDb
              mi <- liftIO $ getUser con (B.unpack l) (B.unpack p)
              case mi of
                Nothing -> blaze "Invalid username or password."
                Just i -> do
                  key <- asks hKey
                  g <- liftIO newStdGen
                  let n = fst $ next g
                      cookie = encrypt key . B.pack $ show n
                  ref <- asks hSessions
                  liftIO $ modifyIORef ref ((n,i):)
                  modifyResponse . addCookie $ Cookie myCookieName cookie Nothing Nothing (Just "/") -- TODO replace the Nothing
                  blaze "Successfuly logged in."

            x -> blaze $
              H.p $ H.string $ "Error when trying to login. " ++ show x

logoutHandler :: Hortened ()
logoutHandler = do
  modifyResponse . addCookie $ Cookie myCookieName "" Nothing Nothing (Just "/")
  blaze "Successfuly logged out."

myCookie :: MonadSnap m => B.ByteString -> m (Maybe Int)
myCookie key = do
  cs <- rqCookies <$> getRequest
  case filter ((== myCookieName) . cookieName) cs of
    [] -> return Nothing
    [x] -> return . toSessionID key $ cookieValue x
    x:_ -> do liftIO $ putStrLn "Two cookies with the same name."
              return . toSessionID key $ cookieValue x

  where toSessionID k bs = case decrypt k bs >>= B.readInt of
          Just (i, bs') | B.null bs' -> Just i
          _ -> Nothing

-- TODO only for debugging
cookiesPage :: Hortened ()
cookiesPage = do
  c <- rqCookies <$> getRequest
  logged <- isLogged
  blaze $ do
    H.h1 "Cookies"
    when logged "Your are logged in."
    H.ul $ mapM_ (H.li . H.string . show) c

short :: Hortened ()
short = do
  ms <- getParam "short"
  con <- asks hDb
  case ms of
    Nothing -> blaze "error"
    Just s -> do
      ml <- liftIO $ getLink con (B.unpack s) True
      case ml of
        Nothing -> blaze . H.string $ "There is nothing here. "
        Just l -> do
          modifyResponse $ setResponseCode 301 -- moved permanently TODO use Snap's redirect' function
          modifyResponse $ setHeader "Location" $ "http://" `B.append` B.pack l

lookupShort :: B.ByteString -> [(Int, B.ByteString)] -> Maybe B.ByteString
lookupShort s = lookup (decode $ B.unpack s)

blaze :: Html -> Hortened ()
blaze response = do
  modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
  writeLBS . renderHtml $ do
    H.docType
    H.meta ! A.charset "UTF-8"
    response
