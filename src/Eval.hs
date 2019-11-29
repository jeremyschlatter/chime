module Eval where

import Data.Bifunctor
import Data.Bits
import Data.Bool
import Data.ByteString as B
import Data.Char
import Data.List.NonEmpty
import Data.Text as T
import Data.Text.Encoding

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
