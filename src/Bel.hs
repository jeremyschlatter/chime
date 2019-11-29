module Bel
  ( module Bel
  , module Text.Megaparsec.Error
  ) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Functor
import Data.List
import Data.List.NonEmpty
import Data.Void

import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as M (parse)
import Text.Megaparsec.Char hiding (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexLit :: String -> Parser ()
lexLit = void <$> L.symbol sc

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
properList (MkPair (car, cdr)) = case cdr of
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

symbol :: Parser Symbol
symbol = (lexeme $ some1 letterChar <&> MkSymbol) <|> try (lexLit "(" *> lexLit ")" $> Nil)

pattern Nil :: Symbol
pattern Nil = MkSymbol ('n' :| "il")

surround :: String -> String -> Parser a -> Parser a
surround a b x = lexLit a *> x <* lexLit b

string :: Parser Object
string = surround "\"" "\"" (listToPair . (fmap Character) <$> many (character' (lexeme letterChar)))

pair :: Parser Pair
pair = surround "(" ")" pair' where
  pair' = liftA2 (curry MkPair) expression $
    (lexLit "." *> expression) <|> (Pair <$> pair') <|> (pure $ Symbol Nil)

character' :: Parser Char -> Parser Character
character' unescaped = fmap MkCharacter $ try (char '\\' *> escaped) <|> unescaped where
  escaped =
  -- @performance: long alternation of parsers here, could be replaced
  --   with a single parser and a case expression or similar
    (foldl (flip \(s, c) -> ((lexLit s $> c) <|>)) empty controlChars)

character :: Parser Character
character = character' $ char '\\' *> lexeme letterChar

expression :: Parser Object
expression =  (Symbol <$> symbol)
          <|> string
          <|> (Pair <$> pair)
          <|> (Character <$> character)

evaluate :: Object -> Object
evaluate = id

parse :: FilePath -> String -> Either (ParseErrorBundle String Void) Object
parse = M.parse (expression <* eof)

repl :: IO ()
repl = forever $ putStr "> " *> getLine >>=
  putStrLn .
  either errorBundlePretty repr .
  (fmap evaluate) .
  parse "[input]"

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
