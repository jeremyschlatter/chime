module Data where

import Control.Monad
import Data.List
import Data.List.NonEmpty

data Object
  = Symbol Symbol
  | Pair Pair
  | Character Character
  | Stream

newtype Symbol = MkSymbol { unSymbol :: NonEmpty Char }
newtype Pair = MkPair { unPair :: (Object, Object) }
newtype Character = MkCharacter { unCharacter :: Char }

pairToList :: Pair -> [Object]
pairToList (MkPair (car, cdr)) = car : case cdr of
  Symbol Nil -> []
  Pair p -> pairToList p
  o -> [o]

listToPair :: [Object] -> Object
listToPair = \case
  [] -> Symbol Nil
  x:xs -> Pair (MkPair (x, listToPair xs))

properList :: Pair -> Bool
properList (MkPair (_, cdr)) = case cdr of
  Symbol Nil -> True
  Pair p -> properList p
  _ -> False

class Repr x where
  repr :: x -> String
instance Repr Symbol where
  repr = toList . unSymbol
instance Repr Pair where
  repr p = maybe
      ("(" <> go p <> ")")
      (\l -> "\"" <> foldMap characterName l <> "\"")
      charList where
    charList :: Maybe [Character]
    charList = guard (properList p) *>
      flip traverse (pairToList p) \case
        Character c -> Just c
        _ -> Nothing
    go (MkPair (car, cdr)) = repr car <> case cdr of
      Symbol Nil -> ""
      Pair p' -> " " <> go p'
      o -> " . " <> repr o
instance Repr Character where
  repr = ("\\" <>) . characterName
instance Repr Object where
  repr = \case
    Symbol s -> repr s
    Pair p -> repr p
    Character c -> repr c
    Stream -> "<stream>"

pattern Nil :: Symbol
pattern Nil = MkSymbol ('n' :| "il")

characterName:: Character -> String
characterName =
  (\c -> maybe (pure c) fst (find ((== c) . snd) controlChars)) .
  unCharacter

controlChars :: [(String, Char)]
controlChars =
  [ ("nul", '\NUL')
  , ("soh", '\SOH')
  , ("stx", '\STX')
  , ("etx", '\ETX')
  , ("eot", '\EOT')
  , ("enq", '\ENQ')
  , ("ack", '\ACK')
  , ("bel", '\BEL')
  , ("dle", '\DLE')
  , ("dc1", '\DC1')
  , ("dc2", '\DC2')
  , ("dc3", '\DC3')
  , ("dc4", '\DC4')
  , ("nak", '\NAK')
  , ("syn", '\SYN')
  , ("etb", '\ETB')
  , ("can", '\CAN')
  , ("del", '\DEL')
  , ("sub", '\SUB')
  , ("esc", '\ESC')
  , ("em", '\EM')
  , ("fs", '\FS')
  , ("gs", '\GS')
  , ("rs", '\RS')
  , ("us", '\US')
  , ("sp", '\SP')
  , ("bs", '\BS')
  , ("ht", '\HT')
  , ("lf", '\LF')
  , ("vt", '\VT')
  , ("ff", '\FF')
  , ("cr", '\CR')
  , ("so", '\SO')
  , ("si", '\SI')
  ]
