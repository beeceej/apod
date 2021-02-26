{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (application) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text
import Lib
import Network.Wai
import Prelude.Compat
import Servant
import System.Environment
import Prelude ()

type ApodAPI =
  "apod" :> Get '[JSON] ApodResponse
    :<|> "static" :> Raw

apodServer :: Server ApodAPI
apodServer = getApod :<|> serveFiles
  where
    getApod = do
      apiKey <- liftIO $ getEnv "APOD_API_KEY"
      liftIO $ runReaderT getApodFromApi (pack apiKey)
    serveFiles = serveDirectoryWebApp "static"

apodAPI :: Proxy ApodAPI
apodAPI = Proxy

application :: Application
application = serve apodAPI apodServer
