module Bel where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Data.List.NonEmpty
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexLit :: String -> Parser ()
lexLit = void <$> L.symbol sc

-- instance (Repr a, Repr b) => Repr (Either a b) where
--   repr = either repr repr
-- instance (ShowErrorComponent e => Repr (ParseErrorBundle s e) where
--   repr = show

data Object
  = Symbol Symbol
  | Pair Pair
  | Character Character
  | Stream

newtype Symbol = MkSymbol { unSymbol :: NonEmpty Char }
newtype Pair = MkPair { unPair :: (Object, Object) }
newtype Character = MkCharacter { unCharacter :: Char }

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

class Repr x where
  repr :: x -> String
instance Repr Symbol where
  repr = toList . unSymbol
instance Repr Pair where
  repr (MkPair (car, cdr)) = "(" <> repr car <> " . " <> repr cdr <> ")"
instance Repr Character where
  repr = ("\\" <>) . maybeLongName . unCharacter where
    maybeLongName c = maybe (pure c) fst (find ((== c) . snd) controlChars)
instance Repr Object where
  repr = \case
    Symbol s -> repr s
    Pair p -> repr p
    Character c -> repr c
    Stream -> "<stream>"

symbol :: Parser Symbol
symbol = lexeme $ some1 letterChar <&> MkSymbol

nil :: Symbol
nil = MkSymbol $ 'n' :| "il"

pair :: Parser Pair
pair = lexLit "(" *> pair' <* lexLit ")" where
  pair' :: Parser Pair
  pair' = liftA2 (curry MkPair) expression $
    (lexLit "." *> expression) <|> (Pair <$> pair') <|> (pure $ Symbol nil)

character :: Parser Character
character = char '\\' *> fmap MkCharacter
  -- @performance: long alternation of parsers here, could be replaced
  --   with a single parser and a case expression or similar
  (foldl (flip \(s, c) -> ((lexLit s $> c) <|>)) (lexeme letterChar) controlChars)

expression :: Parser Object
expression =  (Symbol <$> symbol)
          <|> (Pair <$> pair)
          <|> (Character <$> character)

evaluate :: Object -> Object
evaluate = id

repl :: IO ()
repl = forever $ putStr "> " *> getLine >>=
  putStrLn .
  either errorBundlePretty repr .
  (fmap evaluate) .
  parse (expression <* eof) "[input]"
