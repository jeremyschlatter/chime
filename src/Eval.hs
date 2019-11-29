module Eval where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Bits
import Data.Bool
import qualified Data.ByteString as B
import Data.Char
import Data.List.NonEmpty
import Data.Text as T
import Data.Text.Encoding

import Data
import Parse (parse, errorBundlePretty)

type Error = String

type EvalState = Int

newState :: EvalState
newState = 0

type EvalMonad = ExceptT Error (State EvalState)

readEval :: FilePath -> String -> EvalMonad Object
readEval path =
  (>>= evaluate) .
  except .
  first errorBundlePretty .
  parse path

evaluate :: Object -> EvalMonad Object
evaluate = except . \case
  c@(Character _) -> Right c
  s@Stream -> Right s
  s@(Symbol (MkSymbol (toList -> s'))) -> case s' of
    "nil" -> Right s
    "o" -> Right s
    "apply" -> Right s
    "t" -> Right s
    -- chars is ASCII-only for now
    "chars" -> Right $ listToPair $ flip fmap [0..127] \i ->
      Pair $ MkPair $ (,)
        (Character (MkCharacter (chr i)))
        (listToPair $ convert $ encodeUtf8 (T.singleton (chr i)))
        where
          convert = flip B.foldl [] \acc -> (acc <>) . \w ->
            Character . MkCharacter . bool '0' '1' . testBit w <$> [0..7]

    _ -> Left $ "undefined symbol " <> s'
  Pair _ -> Left $ "pair evaluation not implemented yet"

runEval :: EvalMonad a -> EvalState -> (Either Error a, EvalState)
runEval = runState . runExceptT

readRunEval :: FilePath -> String -> EvalState -> (Either Error Object, EvalState)
readRunEval p c s = flip runEval s $ readEval p c

repl :: IO ()
repl = go newState where
  go s = do
    putStr "> "
    line <- getLine
    let (x, s') = readRunEval "repl" line s
    putStrLn $ either id repr x
    -- loop, resetting state if there was an error
    go $ either (const s) (const s') x
