module Parse
  ( module Parse
  , module Text.Megaparsec.Error
  ) where

import Control.Applicative.Combinators.NonEmpty
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.State.Class
import Data.Bitraversable
import Data.List.NonEmpty as NE hiding (map)
import qualified Data.Map.Strict as Map

import Text.Megaparsec hiding (parse, sepBy1)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char hiding (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

import Common hiding (try, many)
import Data hiding (string, number, o)

type ParseState r = Map.Map Int (r (Pair r))

data BelParseError = BelParseError String deriving (Eq, Ord)

instance ShowErrorComponent BelParseError where
  showErrorComponent (BelParseError e) = e

type Parser m = ParsecT BelParseError String (StateT (ParseState (Ref m)) m)

sc :: Ord e => ParsecT e String m ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser m a -> Parser m a
lexeme = L.lexeme sc

lexLit :: String -> Parser m ()
lexLit = void <$> L.symbol sc

regularChar :: Parser m Char
regularChar = (alphaNumChar <|> oneOf "$%&*+-/;<=>?@^_{}¦") <?> "regular character"

nonVBar :: Parser m Char
nonVBar = (alphaNumChar <|> oneOf "$%&*+-/;<=>?@^_{}") <?> "regular character"

data DotBang a = Dot a | Bang a deriving Functor

composedSymbols :: forall m. MonadRef m => Parser m (Object (Ref m))
composedSymbols = let
  -- regular symbols
  s0 :: Parser m (Object (Ref m))
  s0 = barQuotedSymbol <|> (some1 regularChar <&> Symbol . MkSymbol)
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
  s4 = bisequence (bisequence (optional (dotBang' (pure ())), s3), many (dotBang s3)) >>= \case
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
  dotBang' = ((try (char '.' *> notFollowedBy digitChar $> Dot) <|> (char '!' $> Bang)) <*>)
  compose :: NonEmpty (Object (Ref m)) -> Parser m (Object (Ref m))
  compose = \case
    x :| [] -> pure x
    xs -> pureListToObject $ NE.toList $ Sym 'c' "ompose" <| xs
  in lexeme s5

surround :: String -> String -> Parser m a -> Parser m a
surround a b x = lexLit a *> x <* lexLit b

barQuotedSymbol :: Parser m (Object (Ref m))
barQuotedSymbol = (char '¦' *>) $ (<* lexLit "¦") $
  some1 (nonVBar <|> oneOf "#,`'\\.[](): !~|") <&> Symbol . MkSymbol

string :: MonadRef m => Parser m (Object (Ref m))
string = (char '"' *>) $ (<* lexLit "\"") $
  many (Character <$> character' (regularChar <|> oneOf "#,`'\\.[](): !~|")) >>=
    pureListToObject

pair :: MonadMutableRef m => Parser m (Pair (Ref m))
pair = surround "(" ")" pair' where
  pair' = liftA2 (curry MkPair) expression $
    (lexLit ". " *> expression) <|> (pair' >>= map Pair . newRef) <|> (pure $ Symbol Nil)

character' :: Parser m Char -> Parser m Character
character' unescaped = map MkCharacter $ try (char '\\' *> escaped) <|> unescaped where
  escaped =
  -- @performance: long alternation of parsers here, could be replaced
  --   with a single parser and a case expression or similar
    (foldl (flip \(s, c) -> ((lexLit s $> c) <|>)) empty controlChars)

character :: Parser m Character
character = character' $ char '\\' *> lexeme (regularChar <|> oneOf "#\",`'\\.[]():!~| ")

quoted :: MonadMutableRef m => String -> String -> Parser m (Object (Ref m))
quoted p q = lexLit p *> expression >>= (q .|)

mkPair' :: MonadRef m => Object (Ref m) -> Object (Ref m) -> m (Object (Ref m))
mkPair' a b = map Pair $ newRef $ MkPair $ (a, b)

-- [f _ x] -> (fn (_) (f _ x))
bracketFn :: MonadMutableRef m => Parser m (Object (Ref m))
bracketFn = lexChar '[' *> (many expression >>= wrap) <* lexChar ']' where
  wrap x = l [pure (Sym 'f' "n"), pl [Sym '_' ""], pl x]
  l = listToObject
  pl = pureListToObject
  lexChar = lexeme . char

number :: MonadRef m => Parser m (Object (Ref m))
number = complex >>= toObject where
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
  decimal = (,) <$> opt integer <*> opt decimalPart >>= \case
    (Nothing, Nothing) -> empty
    (z, r) -> pure $ ((fromMaybe 0 z) % 1) + fromMaybe 0 r
  decimalPart :: Parser m Rational
  decimalPart = char '.' *> M.some (read . pure <$> digitChar) <&> sumDecimalDigits 10
  sumDecimalDigits :: Integer -> [Integer] -> Rational
  sumDecimalDigits base = \case
    [] -> 0
    x:xs -> (x % base) + sumDecimalDigits (base * 10) xs
  integer :: Parser m Integer
  integer = L.decimal
  opt p = map Just (try p) <|> pure Nothing

sharedPair :: forall m. MonadMutableRef m => Parser m (Object (Ref m))
sharedPair = char '#' *> L.decimal >>= \n -> tag n <|> ref n where
  tag :: Int -> Parser m (Object (Ref m))
  tag n = findShare n >>= \case
    Just _ -> customFailure $ BelParseError $
      "duplicate definition of shared pair #" <> show n
    Nothing -> char '=' *> newRef (MkPair (Symbol Nil, Symbol Nil)) >>= \r -> do
      modify (Map.insert n r)
      p <- pair
      writeRef r p
      pure (Pair r)
  ref :: Int -> Parser m (Object (Ref m))
  ref n = findShare n >>= \case
    Nothing -> customFailure $ BelParseError $
      "reference to shared pair #" <> show n <> " with no preceding definition"
    Just r -> pure $ Pair r
  findShare n = Map.lookup n <$> get

expression :: MonadMutableRef m => Parser m (Object (Ref m))
expression =  composedSymbols
          <|> lexeme sharedPair
          <|> quoted "'" "quote"
          <|> quoted "`" "bquote"
          <|> quoted ",@" "comma-at"
          <|> quoted "," "comma"
          <|> string
          <|> bracketFn
          <|> (pair >>= map Pair . newRef)
          <|> (Character <$> character)

bom :: Parser m ()
bom = void $ char '\xfeff'

parse :: (MonadMutableRef m, r ~ Ref m)
  => FilePath -> String -> m (Either (ParseErrorBundle String BelParseError) (Object r))
parse = flip evalStateT Map.empty .: M.runParserT (optional bom *> sc *> expression <* eof)

parseMany :: (MonadMutableRef m, r ~ Ref m)
  => FilePath -> String -> m (Either (ParseErrorBundle String BelParseError) [Object r])
parseMany = flip evalStateT Map.empty .: M.runParserT (optional bom *> sc *> many expression <* eof)

isEmptyLine :: String -> Bool
isEmptyLine = isJust . parseMaybe (sc @Void *> eof)
