module Parse
  ( module Parse
  , module Text.Megaparsec.Error
  ) where

import BasePrelude hiding (try, many)
import Data.List.NonEmpty

import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as M (parse)
import Text.Megaparsec.Char hiding (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

import Data hiding (string)

type Parser = Parsec Void String

listToPair' :: [Object Identity] -> Object Identity
listToPair' = runIdentity . listToPair . (fmap Identity)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexLit :: String -> Parser ()
lexLit = void <$> L.symbol sc

symbol :: Parser Symbol
symbol = (lexeme $ some1 letterChar <&> MkSymbol) <|> try (lexLit "(" *> lexLit ")" $> Nil)

surround :: String -> String -> Parser a -> Parser a
surround a b x = lexLit a *> x <* lexLit b

string :: Parser (Object Identity)
string = surround "\"" "\"" (listToPair' . (fmap Character) <$> many (character' (lexeme letterChar)))

pair :: Parser (Pair Identity)
pair = surround "(" ")" pair' where
  pair' = liftA2 (curry MkPair) expression $
    (lexLit "." *> expression) <|> (Pair . Identity <$> pair') <|> (pure $ Symbol Nil)

character' :: Parser Char -> Parser Character
character' unescaped = fmap MkCharacter $ try (char '\\' *> escaped) <|> unescaped where
  escaped =
  -- @performance: long alternation of parsers here, could be replaced
  --   with a single parser and a case expression or similar
    (foldl (flip \(s, c) -> ((lexLit s $> c) <|>)) empty controlChars)

character :: Parser Character
character = character' $ char '\\' *> lexeme letterChar

quotedExpression :: Parser (Object Identity)
quotedExpression = char '\'' *> expression <&> \x ->
  listToPair' [Symbol (MkSymbol ('q' :| "uote")), x]

expression :: Parser (Object Identity)
expression =  (Symbol <$> symbol)
          <|> quotedExpression
          <|> string
          <|> (Pair . Identity <$> pair)
          <|> (Character <$> character)

parse :: (MonadRef m, r ~ Ref m)
  => FilePath -> String -> Either (ParseErrorBundle String Void) (m (Object r))
parse = M.parse (runIdentity . refSwap <$> (sc *> expression <* eof))

parseMany :: (MonadRef m, r ~ Ref m)
  => FilePath -> String -> Either (ParseErrorBundle String Void) (m [Object r])
parseMany = M.parse (traverse (runIdentity . refSwap) <$> (sc *> many expression <* eof))

isEmptyLine :: String -> Bool
isEmptyLine = isJust . parseMaybe (sc *> eof)
