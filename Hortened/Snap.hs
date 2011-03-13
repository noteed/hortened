{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hortened.Snap where

import Control.Applicative ((<$>), Alternative, Applicative)
import Control.Monad (MonadPlus)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, runReaderT, asks)
import Data.Maybe (isJust)

import Snap.Types

import Hortened.Data

-- Equivalent to type Hortened = SnapExtend Hort'd
newtype Hortened a = Hortened (ReaderT H Snap a)
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadIO
    , MonadCatchIO
    , MonadSnap
    , MonadReader H
    )

runHortened :: Hortened a -> H -> Snap a
runHortened (Hortened h) = runReaderT h

isLogged :: Hortened Bool
isLogged = isJust <$> asks hUser

isSecure :: Hortened Bool
isSecure = rqIsSecure <$> getRequest
