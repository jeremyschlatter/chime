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

-- type Parser = Parsec Void String
-- type Parser m = ParsecT Void String (StateT (Shares (Ref m)) m)
type Parser m = ParsecT Void String m

listToPair' :: [Object Identity] -> Object Identity
listToPair' = runIdentity . toObject . (fmap Identity)

o :: (ToObject Identity Identity x) => x -> Object Identity
o = runIdentity . toObject

sc :: Parser m ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser m a -> Parser m a
lexeme = L.lexeme sc

lexLit :: String -> Parser m ()
lexLit = void <$> L.symbol sc

regularChar :: Parser m Char
regularChar = (alphaNumChar <|> oneOf "#$%&*+-/;<=>?@^_{}¦") <?> "regular character"

data DotBang a = Dot a | Bang a deriving Functor

composedSymbols :: Parser m (Object Identity)
composedSymbols = let
  -- regular symbols
  s0 :: Parser m (Object Identity)
  s0 = (some1 regularChar <&> Symbol . MkSymbol)
       <|> try (lexLit "(" *> lexLit ")" $> Symbol Nil)
  -- numbers
  s1 :: Parser m (Object Identity)
  s1 = try number <|> s0
  -- tildes
  s2 :: Parser m (Object Identity)
  s2 = bisequence (optional (char '~'), s1) <&> \case
    (Nothing, x) -> x
    (Just _, x) -> compose $ Sym 'n' "o" :| [x]
  -- colon
  s3 :: Parser m (Object Identity)
  s3 = sepBy1 s2 (char ':') <&> compose
  -- . and !
  s4 :: Parser m (Object Identity)
  s4 = bisequence (bisequence (optional (dotBang (pure ())), s3), many (dotBang s3)) <&> \case
    ((Just f, x), xs) -> go (Sym 'u' "pon") ((f $> x) : xs)
    ((Nothing, x), []) -> x
    ((Nothing, x), xs) -> go x xs
    where
      go :: Object Identity -> [DotBang (Object Identity)] -> Object Identity
      go = curry $ runIdentity . \case
        (a, []) -> a .* "nil"
        (a, Dot b : cs) -> a .* go b cs
        (a, Bang b : cs) -> a .* go (runIdentity $ quote b) cs
  -- |
  s5 :: Parser m (Object Identity)
  s5 = sepBy1 s4 (char '|') <&> go where
    go = \case
      a :| [] -> a
      a :| b:cs -> listToPair' [Sym 't' "", a, go (b:|cs)]
  dotBang :: Parser m a -> Parser m (DotBang a)
  dotBang = (((char '.' $> Dot) <|> (char '!' $> Bang)) <*>)
  compose :: NonEmpty (Object Identity) -> Object Identity
  compose = \case
    x :| [] -> x
    xs -> listToPair' $ NE.toList $ o "compose" <| xs
  in lexeme s5

surround :: String -> String -> Parser m a -> Parser m a
surround a b x = lexLit a *> x <* lexLit b

string :: Parser m (Object Identity)
string = surround "\"" "\"" (listToPair' . (fmap Character) <$>
  many (character' (regularChar <|> oneOf ",`'\\.[](): !~|")))

pair :: Parser m (Pair Identity)
pair = surround "(" ")" pair' where
  pair' = liftA2 (curry MkPair) expression $
    (lexLit "." *> expression) <|> (Pair . Identity <$> pair') <|> (pure $ Symbol Nil)

character' :: Parser m Char -> Parser m Character
character' unescaped = fmap MkCharacter $ try (char '\\' *> escaped) <|> unescaped where
  escaped =
  -- @performance: long alternation of parsers here, could be replaced
  --   with a single parser and a case expression or similar
    (foldl (flip \(s, c) -> ((lexLit s $> c) <|>)) empty controlChars)

character :: Parser m Character
character = character' $ char '\\' *> lexeme (regularChar <|> oneOf "\",`'\\.[]():!~|")

quotedExpression :: Parser m (Object Identity)
quotedExpression = char '\'' *> expression <&> runIdentity . quote

backQuotedList :: Parser m (Object Identity)
backQuotedList = char '`' *> surround "(" ")" (many expression) <&> backquote . listToPair'
  where
    -- @incomplete: implement this in a way that cannot clash with user symbols
    backquote = runIdentity . ("~backquote" .|)

commaExpr :: Parser m (Object Identity)
commaExpr = char ',' *> (atExpr <|> expr) where
  expr = expression <&> \x ->
    -- @incomplete: implement this in a way that cannot clash with user symbols
    Pair $ Identity $ MkPair (Sym '~' "comma", x)
  atExpr = char '@' *> expression <&> \x ->
    Pair $ Identity $ MkPair (Sym '~' "splice", x)

-- [f _ x] -> (fn (_) (f _ x))
bracketFn :: Parser m (Object Identity)
bracketFn = lexChar '[' *> (wrap <$> many expression) <* lexChar ']' where
  wrap x = l [Sym 'f' "n", l [Sym '_' ""], l x]
  l = listToPair'
  lexChar = lexeme . char

number :: Parser m (Object Identity)
number = lexeme (o <$> complex) where
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

expression :: Parser m (Object Identity)
expression =  composedSymbols
          <|> quotedExpression
          <|> backQuotedList
          <|> commaExpr
          <|> string
          <|> bracketFn
          <|> number
          <|> (Pair . Identity <$> pair)
          <|> (Character <$> character)

bom :: Parser m ()
bom = void $ char '\xfeff'

parse :: (MonadRef m, r ~ Ref m)
  => FilePath -> String -> Either (ParseErrorBundle String Void) (m (Object r))
parse = M.parse (runIdentity . refSwap <$>
  (optional bom *> sc *> expression <* eof))

parseMany :: (MonadRef m, r ~ Ref m)
  => FilePath -> String -> Either (ParseErrorBundle String Void) (m [Object r])
parseMany = M.parse (traverse (runIdentity . refSwap) <$>
  (optional bom *> sc *> many expression <* eof))

isEmptyLine :: String -> Bool
isEmptyLine = isJust . parseMaybe (sc *> eof)
