module Bel where

import Control.Monad
import Data.Functor
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

class Repr x where
  repr :: x -> String
instance Repr Symbol where
  repr = toList . unSymbol
instance Repr Pair where
  repr (MkPair (car, cdr)) = "(" <> repr car <> " . " <> repr cdr <> ")"
instance Repr Character where
  repr = pure . unCharacter
instance Repr Object where
  repr = \case
    Symbol s -> repr s
    Pair p -> repr p
    Character c -> repr c
    Stream -> "<stream>"

symbol :: Parser Symbol
symbol = lexeme $ some1 letterChar <&> MkSymbol

pair :: Parser Pair
pair = do
  lexLit "("
  car <- expression
  lexLit "."
  cdr <- expression
  lexLit ")"
  pure $ MkPair (car, cdr)

character :: Parser Character
character = undefined

expression :: Parser Object
expression =  (Symbol <$> symbol)
          <|> (Pair <$> pair)
          <|> (Character <$> character)

evaluate :: Object -> Object
evaluate = id

repl :: IO ()
repl = forever $ putStr "> " *> getLine >>=
  putStrLn . either errorBundlePretty repr . (fmap evaluate) . parse expression "[input]"
