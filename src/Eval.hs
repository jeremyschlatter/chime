module Eval where

import Data.Bifunctor
import Data.List.NonEmpty

import Data
import Parse (parse, errorBundlePretty)

type Error = String

readEval :: FilePath -> String -> Either Error Object
readEval path = (>>= evaluate) . first errorBundlePretty . parse path

evaluate :: Object -> Either Error Object
evaluate = \case
  c@(Character _) -> Right c
  s@Stream -> Right s
  s@(Symbol (MkSymbol (toList -> s'))) -> case s' of
    "nil" -> Right s
    "o" -> Right s
    "apply" -> Right s
    "t" -> Right s
    _ -> Left $ "undefined symbol " <> s'
  Pair _ -> Left $ "pair evaluation not implemented yet"
