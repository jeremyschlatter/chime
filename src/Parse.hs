module Parse
  ( module Parse
  , module Text.Megaparsec.Error
  ) where

import BasePrelude hiding (try, many)
import Control.Applicative.Combinators.NonEmpty
import Data.Bitraversable
import Data.List.NonEmpty as NE

import Text.Megaparsec hiding (parse, sepBy1)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char hiding (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

import Data hiding (string, number, o)

type Parser m = ParsecT Void String m

sc :: Parser m ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser m a -> Parser m a
lexeme = L.lexeme sc

lexLit :: String -> Parser m ()
lexLit = void <$> L.symbol sc

regularChar :: Parser m Char
regularChar = (alphaNumChar <|> oneOf "#$%&*+-/;<=>?@^_{}¦") <?> "regular character"

data DotBang a = Dot a | Bang a deriving Functor

composedSymbols :: forall m. MonadRef m => Parser m (Object (Ref m))
composedSymbols = let
  -- regular symbols
  s0 :: Parser m (Object (Ref m))
  s0 = (some1 regularChar <&> Symbol . MkSymbol)
       <|> try (lexLit "(" *> lexLit ")" $> Symbol Nil)
  -- numbers
  s1 :: Parser m (Object (Ref m))
  s1 = try number <|> s0
  -- tildes
  s2 :: Parser m (Object (Ref m))
  s2 = bisequence (optional (char '~'), s1) >>= \case
    (Nothing, x) -> pure x
    (Just _, x) -> compose $ Sym 'n' "o" :| [x]
  -- colon
  s3 :: Parser m (Object (Ref m))
  s3 = sepBy1 s2 (char ':') >>= compose
  -- . and !
  s4 :: Parser m (Object (Ref m))
  s4 = bisequence (bisequence (optional (dotBang (pure ())), s3), many (dotBang s3)) >>= \case
    ((Just f, x), xs) -> go (Sym 'u' "pon") ((f $> x) : xs)
    ((Nothing, x), []) -> pure x
    ((Nothing, x), xs) -> go x xs
    where
      go :: Object (Ref m) -> [DotBang (Object (Ref m))] -> Parser m (Object (Ref m))
      go = curry \case
        (a, []) -> a .* "nil"
        (a, Dot b : cs) -> a .* go b cs
        (a, Bang b : cs) -> quote b >>= \b' -> a .* go b' cs
  -- |
  s5 :: Parser m (Object (Ref m))
  s5 = sepBy1 s4 (char '|') >>= go where
    go = \case
      a :| [] -> pure a
      a :| b:cs -> listToObject [pure (Sym 't' ""), pure a, go (b:|cs)]
  dotBang :: Parser m a -> Parser m (DotBang a)
  dotBang = (((char '.' $> Dot) <|> (char '!' $> Bang)) <*>)
  compose :: NonEmpty (Object (Ref m)) -> Parser m (Object (Ref m))
  compose = \case
    x :| [] -> pure x
    xs -> pureListToObject $ NE.toList $ Sym 'c' "ompose" <| xs
  in lexeme s5

surround :: String -> String -> Parser m a -> Parser m a
surround a b x = lexLit a *> x <* lexLit b

string :: MonadRef m => Parser m (Object (Ref m))
string = surround "\"" "\"" $
  many (Character <$> character' (regularChar <|> oneOf ",`'\\.[](): !~|")) >>=
    pureListToObject

pair :: MonadRef m => Parser m (Pair (Ref m))
pair = surround "(" ")" pair' where
  pair' = liftA2 (curry MkPair) expression $
    (lexLit "." *> expression) <|> (pair' >>= fmap Pair . newRef) <|> (pure $ Symbol Nil)

character' :: Parser m Char -> Parser m Character
character' unescaped = fmap MkCharacter $ try (char '\\' *> escaped) <|> unescaped where
  escaped =
  -- @performance: long alternation of parsers here, could be replaced
  --   with a single parser and a case expression or similar
    (foldl (flip \(s, c) -> ((lexLit s $> c) <|>)) empty controlChars)

character :: Parser m Character
character = character' $ char '\\' *> lexeme (regularChar <|> oneOf "\",`'\\.[]():!~|")

quotedExpression :: MonadRef m => Parser m (Object (Ref m))
quotedExpression = char '\'' *> expression >>= quote

backQuotedList :: MonadRef m => Parser m (Object (Ref m))
backQuotedList = char '`' *> surround "(" ")" (many expression) >>= pureListToObject >>=
  -- @incomplete: implement this in a way that cannot clash with user symbols
  ("~backquote" .|)

mkPair' :: MonadRef m => Object (Ref m) -> Object (Ref m) -> m (Object (Ref m))
mkPair' a b = fmap Pair $ newRef $ MkPair $ (a, b)

commaExpr :: MonadRef m => Parser m (Object (Ref m))
commaExpr = char ',' *> (atExpr <|> expr) where
  -- @incomplete: implement this in a way that cannot clash with user symbols
  expr = expression >>= mkPair' (Sym '~' "comma")
  atExpr = char '@' *> expression >>= mkPair' (Sym '~' "splice")

-- [f _ x] -> (fn (_) (f _ x))
bracketFn :: MonadRef m => Parser m (Object (Ref m))
bracketFn = lexChar '[' *> (many expression >>= wrap) <* lexChar ']' where
  wrap x = l [pure (Sym 'f' "n"), pl [Sym '_' ""], pl x]
  l = listToObject
  pl = pureListToObject
  lexChar = lexeme . char

number :: MonadRef m => Parser m (Object (Ref m))
number = lexeme (complex >>= toObject) where
  complex :: Parser m (Complex Rational)
  complex = bisequence (opt rational, opt (rational <* char 'i')) >>= \case
    (Nothing, Nothing) -> empty
    (Just r, Nothing) -> pure (r :+ 0)
    (Nothing, Just i) -> pure (0 :+ i)
    (Just r,  Just i) -> pure (r :+ i)
  rational :: Parser m Rational
  rational = L.signed (pure ()) (try ratio <|> decimal)
  ratio :: Parser m Rational
  ratio = bisequence (integer, char '/' *> integer) <&> uncurry (%)
  decimal :: Parser m Rational
  decimal = toRational <$> L.scientific
  integer :: Parser m Integer
  integer = L.decimal
  opt p = fmap Just p <|> pure Nothing

expression :: MonadRef m => Parser m (Object (Ref m))
expression =  composedSymbols
          <|> quotedExpression
          <|> backQuotedList
          <|> commaExpr
          <|> string
          <|> bracketFn
          <|> number
          <|> (pair >>= fmap Pair . newRef)
          <|> (Character <$> character)

bom :: Parser m ()
bom = void $ char '\xfeff'

parse :: (MonadRef m, r ~ Ref m)
  => FilePath -> String -> m (Either (ParseErrorBundle String Void) (Object r))
parse = M.runParserT (optional bom *> sc *> expression <* eof)

parseMany :: (MonadRef m, r ~ Ref m)
  => FilePath -> String -> m (Either (ParseErrorBundle String Void) [Object r])
parseMany = M.runParserT (optional bom *> sc *> many expression <* eof)

isEmptyLine :: String -> Bool
isEmptyLine = isJust . parseMaybe (sc *> eof)
