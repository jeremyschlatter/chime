{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings #-}
module Main where

import BasePrelude
import Codec.Compression.Zstd.Lazy
import Data.Aeson hiding (json)
import qualified Data.ByteString.Base64.Lazy as Base64
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Web.Scotty

import CacheBelDotBel
import Data
import Eval

belDotBelState :: String
belDotBelState = $(serializedBelDotBelState)

data StatefulRequest = StatefulRequest
  { expr :: String
  , state :: Text
  } deriving (Generic, FromJSON)

data StatefulResponse = StatefulResponse
  { result :: String
  , state :: Text
  } deriving (Generic, ToJSON)

compressState :: String -> Text
compressState = decodeUtf8 . Base64.encode . compress 1 . encodeUtf8 . pack

decompressState :: Text -> String
decompressState = unpack . decodeUtf8 . decompress . Base64.decodeLenient . encodeUtf8

main :: IO ()
main = stringToState belDotBelState >>= either die pure >>= \baseState ->
  (read . fromMaybe "8080" <$> lookupEnv "PORT") >>= flip scotty do

    post "/" do
      addHeader "Access-Control-Allow-Origin" "*"
      body >>= liftIO . flip (readThenRunEval "") baseState . unpack . decodeUtf8
        >>= either pure repr . fst >>= text . pack

    post "/stateful" do
      addHeader "Access-Control-Allow-Origin" "*"
      body >>= (. decode) \case
        Nothing -> text "malformed request"
        Just req -> do
          hydrated <- case state (req :: StatefulRequest) of
            "" -> pure baseState
            s -> stringToState (decompressState s) >>=
              either ((*> finish) . text . pack . ("malformed state: " <>)) pure
          (r, s) <- liftIO $ readThenRunEval "" (expr req) hydrated
          r' <- either pure repr r
          s' <- compressState <$> stateToString s
          json $ StatefulResponse {result=r', state=s'}

    get "/" $ text "Chime API Server - send me Bel code and get eval results!"
