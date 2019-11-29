module Main where

import Control.Monad
import System.IO

import Data
import Eval

main :: IO ()
main = hSetBuffering stdout NoBuffering *> repl

repl :: IO ()
repl = forever $ putStr "> " *> getLine >>=
  putStrLn .
  either id repr .
  readEval "[input]"
