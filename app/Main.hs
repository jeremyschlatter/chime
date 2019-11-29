module Main where

import System.IO

import Eval

main :: IO ()
main = hSetBuffering stdout NoBuffering *> repl
