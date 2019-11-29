module Parse
  ( module Parse
  , module Text.Megaparsec.Error
  ) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Functor
import Data.List.NonEmpty
import Data.Void

import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as M (parse)
import Text.Megaparsec.Char hiding (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

import Data hiding (string)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexLit :: String -> Parser ()
lexLit = void <$> L.symbol sc

symbol :: Parser Symbol
symbol = (lexeme $ some1 letterChar <&> MkSymbol) <|> try (lexLit "(" *> lexLit ")" $> Nil)

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

parse :: FilePath -> String -> Either (ParseErrorBundle String Void) Object
parse = M.parse (expression <* eof)
