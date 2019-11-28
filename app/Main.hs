module Main where

import System.IO

import Bel

main :: IO ()
main = hSetBuffering stdout NoBuffering *> repl
