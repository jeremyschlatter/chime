{-# LANGUAGE UndecidableInstances #-}
module Data where

import BasePrelude
import Control.Monad.Trans.Class

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

data Object r
  = Symbol Symbol
  | Pair (r (Pair r))
  | Character Character
  | Stream
  | WhereResult (Object r)

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
      Pair <$> (newRef $ MkPair (car'', cdr''))

newtype Symbol = MkSymbol { unSymbol :: NonEmpty Char } deriving Eq
newtype Pair m = MkPair { unPair :: (Object m, Object m) }
newtype Character = MkCharacter { unCharacter :: Char } deriving Eq

properList :: (MonadRef m, r ~ Ref m) => Object r -> m (Maybe [Object r])
properList = \case
  Symbol Nil -> pure (Just [])
  Pair ref -> readRef ref >>= \(MkPair (car, cdr)) -> fmap (car :) <$> properList cdr
  _ -> pure Nothing

string :: (MonadRef m, r ~ Ref m) => Object r -> m (Maybe [Character])
string x = properList x <&> (=<<) (traverse \case
    Character c -> Just c
    _ -> Nothing
  )

listToPair :: (MonadRef m, r ~ Ref m) => [m (Object r)] -> m (Object r)
listToPair = \case
  [] -> pure $ Symbol Nil
  x:xs -> do
    car <- x
    cdr <- listToPair xs
    Pair <$> newRef (MkPair (car, cdr))

class Repr r x where
  repr :: (MonadRef m, r ~ Ref m) => x -> m String
instance Repr r Symbol where
  repr = pure . toList . unSymbol
instance Repr r' (r' (Pair r')) where
  repr :: forall r m. (MonadRef m, r ~ Ref m) => r (Pair r) -> m String
  repr ref = do
    mb <- string (Pair ref)
    s <- go ref
    pure $ maybe
      ("(" <> s <> ")")
      (\l -> "\"" <> foldMap escaped l <> "\"")
      mb
    where
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
