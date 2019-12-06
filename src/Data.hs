{-# LANGUAGE UndecidableInstances #-}
module Data where

import BasePrelude
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Bitraversable

import Common

-- MonadRef models mutable variables.
-- Mutable variables are a core part of the Bel data model.
-- During evaluation, m will be IO. But we don't need or want
-- the full power of IO here, so we restrict our interface to
-- reading and writing references.
class Monad m => MonadRef m where
  type Ref m :: * -> *
  readRef :: Ref m a -> m a
  newRef :: a -> m (Ref m a)

instance MonadRef IO where
  type Ref IO = IORef
  readRef = readIORef
  newRef = newIORef

instance MonadRef Identity where
  type Ref Identity = Identity
  readRef = id
  newRef = Identity . Identity

instance (MonadRef m, MonadTrans t, Monad (t m)) => MonadRef (t m) where
  type Ref (t m) = Ref m
  readRef = lift . readRef
  newRef = lift . newRef

type Variable = Either (IORef (Pair IORef)) Symbol

instance Repr r (Either (r (Pair r)) Symbol) where
  repr = either repr repr
instance ToObject m r (Either (r (Pair r)) Symbol) where
  toObject = pure . either Pair Symbol

data Object r
  = Symbol Symbol
  | Pair (r (Pair r))
  | Character Character
  | Stream
  | WhereResult (Object r)

class ToObject m r x where
  toObject :: (MonadRef m, r ~ Ref m) => x -> m (Object r)
instance ToObject m r Symbol where
  toObject = pure . Symbol
instance ToObject m r Char where
  toObject = pure . Character . MkCharacter
instance ToObject m r String where
  toObject = \case
    [] -> error "developer error: tried to convert empty string to symbol"
    s:tring -> pure $ Symbol $ MkSymbol $ s:|tring
instance ToObject m r (Object r) where
  toObject = pure
instance ToObject m r (m (Object r)) where
  toObject = id
instance ToObject m r [m (Object r)] where
  toObject = \case
    [] -> toObject "nil"
    x:xs -> x .* toObject @m @r xs

-- toObject specialized to [m (Object r)]
listToObject :: (MonadRef m, r ~ Ref m) => [m (Object r)] -> m (Object r)
listToObject = toObject

(.*) :: forall m r a b. (MonadRef m, r ~ Ref m, ToObject m r a, ToObject m r b)
     => a -> b -> m (Object r)
(.*) = fmap Pair . ((newRef . MkPair) =<<) . bimapM (toObject @m) (toObject @m) .: (,)
infixr 4 .*
(.|) :: forall m r a b. (MonadRef m, r ~ Ref m, ToObject m r a, ToObject m r b)
     => a -> b -> m (Object r)
a .| b = a .* ((.*) @m b "nil") where
infixr 4 .|

refSwap :: (MonadRef a, MonadRef b, ra ~ Ref a, rb ~ Ref b) => Object ra -> a (b (Object rb))
refSwap = \case
  WhereResult x -> WhereResult <$$> refSwap x
  Symbol s -> pure $ pure $ Symbol s
  Character c -> pure $ pure $ Character c
  Stream -> pure $ pure Stream
  Pair r -> do
    MkPair (car, cdr) <- readRef r
    car' <- refSwap car
    cdr' <- refSwap cdr
    pure $ do
      car'' <- car'
      cdr'' <- cdr'
      car'' .* cdr''

newtype Symbol = MkSymbol { unSymbol :: NonEmpty Char } deriving Eq
newtype Pair r = MkPair { unPair :: (Object r, Object r) }
newtype Character = MkCharacter { unCharacter :: Char } deriving Eq

properList1 :: (MonadRef m, r ~ Ref m) => r (Pair r) -> m (Maybe (NonEmpty (Object r)))
properList1 ref = readRef ref >>=
  \(MkPair (car, cdr)) -> fmap (car :|) <$> properList cdr

properList :: (MonadRef m, r ~ Ref m) => Object r -> m (Maybe [Object r])
properList = \case
  Symbol Nil -> pure (Just [])
  Pair ref -> toList <$$> properList1 ref
  _ -> pure Nothing

properListOf :: (MonadRef m, r ~ Ref m) => Object r -> (Object r -> MaybeT m a) -> MaybeT m [a]
properListOf x f = MaybeT (properList x) >>= traverse f

string :: (MonadRef m, r ~ Ref m) => Object r -> m (Maybe [Character])
string x = runMaybeT $ properListOf x \case
  Character c -> pure c
  _ -> empty

quote :: (MonadRef m, r ~ Ref m) => Object r -> m (Object r)
quote = ("quote" .|)

pattern Sym       :: Char -> String -> Object r
pattern Sym n ame = Symbol (MkSymbol (n :| ame))

quoted :: (MonadRef m, r ~ Ref m) => r (Pair r) -> m (Maybe (Object r))
quoted r = runMaybeT $ MaybeT (properList1 r) >>= \case
  Sym 'q' "uote" :| [x] -> pure x
  _ -> empty

class Repr r x where
  repr :: (MonadRef m, r ~ Ref m) => x -> m String
instance Repr r Symbol where
  repr = pure . toList . unSymbol
instance Repr r' (r' (Pair r')) where
  repr :: forall r m. (MonadRef m, r ~ Ref m) => r (Pair r) -> m String
  repr ref = do
    mb <- runMaybeT $
      (MaybeT (string (Pair ref) <&&> \l -> "\"" <> foldMap escaped l <> "\""))
      <|> maybeQuoted "quote" "'"
      <|> maybeQuoted "~backquote" "`"
      <|> maybeQuoted' "~comma" ","
      <|> maybeQuoted' "~splice" ",@"
    go ref <&> \s -> maybe ("(" <> s <> ")") id mb
    where
      maybeQuoted :: String -> String -> MaybeT m String
      maybeQuoted name p = MaybeT (properList1 ref) >>= \case
        Sym n ame :| [x] | n:ame == name -> repr x <&> (p <>)
        _ -> empty
      maybeQuoted' name p = readRef ref >>= \case
        MkPair (Sym n ame, x) | n:ame == name -> repr x <&> (p <>)
        _ -> empty
      escaped :: Character -> String
      escaped c = maybe (pure $ unCharacter c) ("\\" <>) (escapeSequence c)
      go :: (r (Pair r)) -> m String
      go ref' = do
        MkPair (car, cdr) <- readRef ref'
        car' <- repr car
        (car' <>) <$> case cdr of
          Symbol Nil -> pure ""
          Pair p' -> (" " <>) <$> go p'
          o -> (" . " <>) <$> repr o
instance Repr m Character where
  repr c = pure $ "\\" <> maybe (pure (unCharacter c)) id (escapeSequence c)
instance Repr m (Object m) where
  repr = \case
    WhereResult x -> repr x
    Symbol s -> repr s
    Pair p -> repr p
    Character c -> repr c
    Stream -> pure "<stream>"

pattern Nil :: Symbol
pattern Nil = MkSymbol ('n' :| "il")

escapeSequence :: Character -> Maybe String
escapeSequence =
  (\c -> fst <$> (find ((== c) . snd) controlChars)) .
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
