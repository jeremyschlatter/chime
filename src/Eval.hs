module Eval where

import Control.Lens.Combinators
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

type Environment = [(Symbol, Object)]

data EvalState = EvalState
 { _globe :: Environment
 , _scope :: Environment
 }
$(makeLenses ''EvalState)

newState :: EvalState
newState = EvalState [] []

type EvalMonad = ExceptT Error (State EvalState)

readEval :: FilePath -> String -> EvalMonad Object
readEval path =
  (>>= evaluate) .
  except .
  first errorBundlePretty .
  parse path

evaluate :: Object -> EvalMonad Object
evaluate = \case
  c@(Character _) -> pure c
  s@Stream -> pure s
  s@(Symbol (MkSymbol (toList -> s'))) -> case s' of
    "nil" -> pure s
    "o" -> pure s
    "apply" -> pure s
    "t" -> pure s
    -- chars is ASCII-only for now
    "chars" -> pure $ listToPair $ flip fmap [0..127] \i ->
      Pair $ MkPair $ (,)
        (Character (MkCharacter (chr i)))
        (listToPair $ convert $ encodeUtf8 (T.singleton (chr i)))
        where
          convert = flip B.foldl [] \acc -> (acc <>) . \w ->
            Character . MkCharacter . bool '0' '1' . testBit w <$> [0..7]
    "globe" -> getEnv globe
    "scope" -> getEnv scope
    "ins" -> pure (Symbol Nil)
    "outs" -> pure (Symbol Nil)
    _ -> throwE $ "undefined symbol " <> s'
    where
      getEnv =
        (fmap (listToPair . (fmap (Pair . MkPair . first Symbol)))) .
        use
  Pair _ -> throwE $ "pair evaluation not implemented yet"

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
