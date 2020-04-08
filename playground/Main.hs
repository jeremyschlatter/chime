{-# LANGUAGE OverloadedStrings #-}
module Main where

import BasePrelude
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Web.Scotty

import CacheBelDotBel
import Data
import Eval

belDotBelState :: String
belDotBelState = $(serializedBelDotBelState)

main :: IO ()
main = stringToState belDotBelState >>= either die pure >>= withNativeFns >>= \baseState ->
  (read . fromMaybe "8080" <$> lookupEnv "PORT") >>= flip scotty do

    post "/" do
      body >>= liftIO . flip (readThenRunEval "") baseState . unpack . decodeUtf8
        >>= either pure repr . fst >>= text . pack

    get "/" $ text $ pack "Chime API Server - send me Bel code and get eval results!"
