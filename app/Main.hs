module Main where

import BasePrelude
import System.IO

import Eval

main :: IO ()
main = hSetBuffering stdout NoBuffering *> repl
