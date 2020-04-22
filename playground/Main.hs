{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings #-}
module Main where

import BasePrelude
import Codec.Compression.Zstd.Lazy
import Data.Aeson hiding (json)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Delta as Diff
import Web.Scotty
import Web.Scotty.Trans (ActionT)

import Data.Text
import Data.Text.Encoding

import qualified Data.ByteString.Base64.Lazy as Lazy
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyText
import qualified Data.Text.Lazy.Encoding as Lazy

import CacheBelDotBel
import Data as Data
import Eval

belDotBelState :: String
belDotBelState = $(serializedBelDotBelState)

data StatefulRequest = StatefulRequest
  { expr :: String
  , state :: LazyText.Text
  } deriving (Generic, FromJSON)

data StatefulResponse = StatefulResponse
  { result :: String
  , state :: LazyText.Text
  } deriving (Generic, ToJSON)

belDotBelStateBytes :: ByteString.ByteString
belDotBelStateBytes = encodeUtf8 $ pack belDotBelState

compressState :: LazyText.Text -> LazyText.Text
compressState =
  Lazy.decodeUtf8
  . Lazy.encode
  . compress 1
  . LazyBytes.fromStrict
  . Diff.diff belDotBelStateBytes
  . LazyBytes.toStrict
  . Lazy.encodeUtf8

decompressState :: LazyText.Text -> String
decompressState =
  unpack
  . decodeUtf8
  . fromRight ByteString.empty . Diff.patch belDotBelStateBytes
  . LazyBytes.toStrict
  . decompress
  . Lazy.decodeLenient
  . Lazy.encodeUtf8

stateful ::
  EvalState
  -> (String -> String -> EvalState -> IO (Either String (Data.Object IORef), EvalState))
  -> ActionT LazyText.Text IO ()
stateful baseState f = do
  addHeader "Access-Control-Allow-Origin" "*"
  body >>= (. decode) \case
    Nothing -> text "malformed request"
    Just req -> do
      hydrated <- case state (req :: StatefulRequest) of
        "" -> pure baseState
        s -> stringToState (decompressState s) >>=
          either
            ((*> finish) . text . LazyText.fromStrict . pack . ("malformed state: " <>))
            pure
      (r, s) <- liftIO $ f "" (expr req) hydrated
      r' <- either pure repr r
      s' <- compressState . LazyText.toLazyText  <$> (stateToObject s >>= repr')
      json $ StatefulResponse {result=r', state=s'}

main :: IO ()
main = stringToState belDotBelState >>= either die pure >>= \baseState ->
  (read . fromMaybe "8080" <$> lookupEnv "PORT") >>= flip scotty do

    post "/" do
      addHeader "Access-Control-Allow-Origin" "*"
      body >>= liftIO . flip (readThenRunEval "") baseState . unpack
          . decodeUtf8 . LazyBytes.toStrict
        >>= either pure repr . fst >>= text . LazyText.fromStrict . pack

    post "/stateful" $ stateful baseState readThenRunEval

    post "/stateful-long" $ stateful baseState readManyThenRunEval

    get "/" $ text "Chime API Server - send me Bel code and get eval results!"
