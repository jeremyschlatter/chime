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
-- main = builtinsIO >>= \baseState ->
  scotty 3000 $
    post "/" $ do
      body >>= liftIO . flip (readThenRunEval "") baseState . unpack . decodeUtf8
        >>= either pure repr . fst >>= text . pack
