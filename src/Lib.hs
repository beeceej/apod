{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getApodFromApi,
    ApodResponse (..),
  )
where

import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Text
import GHC.Generics
import Network.HTTP.Req
import System.Environment

data ApodResponse = ApodResponse
  { date :: Text,
    explanation :: Text,
    hdurl :: Text,
    media_type :: Text,
    service_version :: Text,
    title :: Text,
    url :: Text
  }
  deriving (Generic, Show)

instance ToJSON ApodResponse where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ApodResponse

getApodFromApi :: ReaderT Text IO ApodResponse
getApodFromApi = do
  apiKey <- ask
  runReq defaultHttpConfig $ do
    r <- req GET (https "api.nasa.gov" /: "planetary" /: "apod") NoReqBody jsonResponse $ "api_key" =: apiKey
    return (responseBody r :: ApodResponse)
