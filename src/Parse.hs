module Parse
  ( module Parse
  , module Text.Megaparsec.Error
  ) where

import BasePrelude hiding (try, many)
import Control.Applicative.Combinators.NonEmpty
import Data.Bitraversable
import Data.List.NonEmpty as NE

import Text.Megaparsec hiding (parse, sepBy1)
import qualified Text.Megaparsec as M (parse)
import Text.Megaparsec.Char hiding (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

import Data hiding (string, number, o)

type Parser = Parsec Void String

listToPair' :: [Object Identity] -> Object Identity
listToPair' = runIdentity . toObject . (fmap Identity)

o :: (ToObject Identity Identity x) => x -> Object Identity
o = runIdentity . toObject

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexLit :: String -> Parser ()
lexLit = void <$> L.symbol sc

regularChar :: Parser Char
regularChar = (alphaNumChar <|> oneOf "!#$%&*+-/;<=>?@^_{|}~¦") <?> "regular character"

composedSymbols :: Parser (Object Identity)
composedSymbols = sepBy1 tildeSymbol (char ':') <&> compose where
  tildeSymbol :: Parser (Object Identity)
  tildeSymbol = bisequence (optional (char '~'), symbol) <&> \case
    (Nothing, x) -> x
    (Just _, x) -> compose $ Sym 'n' "o" :| [x]
  symbol :: Parser (Object Identity)
  symbol = (lexeme $ some1 regularChar <&> Symbol . MkSymbol)
        <|> try (lexLit "(" *> lexLit ")" $> Symbol Nil)
  compose :: NonEmpty (Object Identity) -> Object Identity
  compose = \case
    x :| [] -> x
    xs -> listToPair' $ NE.toList $ o "compose" <| xs

surround :: String -> String -> Parser a -> Parser a
surround a b x = lexLit a *> x <* lexLit b

string :: Parser (Object Identity)
string = surround "\"" "\"" (listToPair' . (fmap Character) <$>
  many (character' (regularChar <|> oneOf ",`'\\.[](): ")))

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
character = character' $ char '\\' *> lexeme (regularChar <|> oneOf "\",`'\\.[]():")

quotedExpression :: Parser (Object Identity)
quotedExpression = char '\'' *> expression <&> runIdentity . quote

backQuotedList :: Parser (Object Identity)
backQuotedList = char '`' *> surround "(" ")" (many expression) <&> backquote . listToPair'
  where
    -- @incomplete: implement this in a way that cannot clash with user symbols
    backquote = runIdentity . ("~backquote" .|)

commaExpr :: Parser (Object Identity)
commaExpr = char ',' *> (atExpr <|> expr) where
  expr = expression <&> \x ->
    -- @incomplete: implement this in a way that cannot clash with user symbols
    Pair $ Identity $ MkPair (Sym '~' "comma", x)
  atExpr = char '@' *> expression <&> \x ->
    Pair $ Identity $ MkPair (Sym '~' "splice", x)

-- [f _ x] -> (fn (_) (f _ x))
bracketFn :: Parser (Object Identity)
bracketFn = lexChar '[' *> (wrap <$> many expression) <* lexChar ']' where
  wrap x = l [Sym 'f' "n", l [Sym '_' ""], l x]
  l = listToPair'
  lexChar = lexeme . char

number :: Parser (Object Identity)
number = lexeme (o <$> complex) where
  complex :: Parser (Complex Rational)
  complex = bisequence (opt rational, opt (rational <* char 'i')) >>= \case
    (Nothing, Nothing) -> empty
    (Just r, Nothing) -> pure (r :+ 0)
    (Nothing, Just i) -> pure (0 :+ i)
    (Just r,  Just i) -> pure (r :+ i)
  rational :: Parser Rational
  rational = L.signed (pure ()) (try ratio <|> decimal)
  ratio :: Parser Rational
  ratio = bisequence (integer, char '/' *> integer) <&> uncurry (%)
  decimal :: Parser Rational
  decimal = toRational <$> L.scientific
  integer :: Parser Integer
  integer = L.decimal
  opt p = fmap Just p <|> pure Nothing

expression :: Parser (Object Identity)
expression =  (try number)
          <|> composedSymbols
          <|> quotedExpression
          <|> backQuotedList
          <|> commaExpr
          <|> string
          <|> bracketFn
          <|> number
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
