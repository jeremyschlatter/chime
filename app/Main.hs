module Main where

import BasePrelude
import System.IO

import CacheBelDotBel
import Common
import Eval

belDotBelState :: String
belDotBelState = $(serializedBelDotBelState)

main :: IO ()
main = hSetBuffering stdout NoBuffering *> stringToState belDotBelState >>=
  either (interpreterBug . ("failed to parse bel.bel state: " <>)) pure >>= repl
