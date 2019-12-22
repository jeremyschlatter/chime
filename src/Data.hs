{-# LANGUAGE UndecidableInstances #-}
module Data where

import BasePrelude
import Control.Lens.Combinators (makeLenses)
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State hiding (get, put)
import Data.Bitraversable
import System.IO

import Common

-- -----------------------------------------------------------------------

type Number = Complex Rational

newtype Symbol = MkSymbol { unSymbol :: NonEmpty Char } deriving Eq

newtype Character = MkCharacter { unCharacter :: Char } deriving Eq

data Pair r
  = MkPair (Object r, Object r)
  | Number Number
  | Continuation (Object IORef -> EvalMonad (Object IORef))
  | OptimizedFunction (OptimizedFunction r)

data Direction = In | Out deriving Eq

data Stream = MkStream
  { streamHandle :: Handle
  , streamDirection :: Direction
  , streamBuf :: Word8
  , streamPlace :: Int
  }

data OptimizedFunction r = MkOptimizedFunction
  { fnBody :: [Object IORef] -> EvalMonad (Object IORef)
  , fnFallback :: (Object r, Object r)
  }

data Object r
  = Symbol Symbol
  | Pair (r (Pair r))
  | Character Character
  | Stream (r Stream)

type Error = String

type Environment = [IORef (Pair IORef)]

type Variable = Either (IORef (Pair IORef)) Symbol

type EvalMonad = ExceptT Error (ContT (Either Error (Object IORef)) (StateT EvalState IO))

data EvalState = EvalState
 { _globe :: Environment
 , _scope :: NonEmpty Environment
 , _locs :: [Maybe Bool]
 , _dyns :: Environment
 , _debug :: [String]
 , _vmark :: IORef (Pair IORef)
 , _ins :: IORef Stream
 , _outs :: IORef Stream
 }
$(makeLenses ''EvalState)

newStream :: (MonadRef m, Ref m ~ IORef) => Direction -> Handle -> m (IORef Stream)
newStream d h = newRef (MkStream h d 0 7)

emptyState :: (MonadRef m, Ref m ~ IORef) => m EvalState
emptyState = EvalState [] (pure []) [] [] []
  <$> newRef (MkPair (Symbol Nil, Symbol Nil))
  <*> newStream In stdin
  <*> newStream Out stdout


-- -----------------------------------------------------------------------
-- The read-only interface to mutable refs

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

-- -----------------------------------------------------------------------
-- The full read-write interface to mutable refs

class MonadRef m => MonadMutableRef m where
  modifyRef :: Ref m a -> (a -> a) -> m ()
  writeRef :: Ref m a -> a -> m ()

instance MonadMutableRef IO where
  modifyRef = modifyIORef
  writeRef = writeIORef

instance (MonadMutableRef m, MonadTrans t, Monad (t m)) => MonadMutableRef (t m) where
  modifyRef = lift .: modifyRef
  writeRef = lift .: writeRef

-- -----------------------------------------------------------------------

readPair :: (MonadMutableRef m, r ~ Ref m) => String -> r (Pair r) -> m (Object r, Object r)
readPair _why x = readRef x >>= \case
  MkPair p -> pure p
  -- Collapse the optimized representation! :(
  -- Number n -> interpreterBug $ "tried to collapse number " <> show n <> " " <> _why
  Number n -> collapseNumber n >>= \p -> (writeRef x (MkPair p)) $> p
  Continuation _ -> pure (Symbol Nil, Symbol Nil)
  -- Collapse the optimized representation! :(
  OptimizedFunction f -> writeRef x (MkPair (fnFallback f)) $> fnFallback f

instance Repr r (Either (r (Pair r)) Symbol) where
  repr = either repr repr
instance ToObject m r (Either (r (Pair r)) Symbol) where
  toObject = pure . either Pair Symbol

o :: (MonadRef m, r ~ Ref m, ToObject m r x) => x -> m (Object r)
o = toObject

mkPair :: (MonadRef m, IORef ~ Ref m) => Object IORef -> Object IORef -> m (IORef (Pair IORef))
mkPair a b = newRef $ MkPair $ (a, b)

class ToObject m r x where
  toObject :: (MonadRef m, r ~ Ref m) => x -> m (Object r)
instance ToObject m r Symbol where
  toObject = pure . Symbol
instance ToObject m r Char where
  toObject = pure . Character . MkCharacter
instance ToObject m r String where
  toObject = \case
    [] -> interpreterBug "tried to convert empty string to symbol"
    s:tring -> pure $ Symbol $ MkSymbol $ s:|tring
instance ToObject m r (Object r) where
  toObject = pure
instance ToObject m r (m (Object r)) where
  toObject = id
instance ToObject m r [m (Object r)] where
  toObject = \case
    [] -> toObject "nil"
    x:xs -> x .* toObject @m @r xs
instance ToObject m r (Complex Rational) where
  toObject n = fmap Pair $ newRef $ Number n
instance ToObject m r Bool where
  toObject = pure . \case
    True -> Sym 't' ""
    False -> Symbol Nil

collapseNumber
  :: forall m r. (MonadRef m, r ~ Ref m)
  => Complex Rational -> m (Object r, Object r)
collapseNumber c = (Sym 'l' "it",) <$> o [o "num", num $ realPart c, num $ imagPart c] where
    num :: Rational -> m (Object r)
    num n = o [sign n, unary $ numerator $ abs n, unary $ denominator $ abs n]
    unary :: Integer -> m (Object r)
    -- @incomplete: generalize this to handle things outside Int range
    unary = listToObject . flip replicate (o "t") .  fromInteger
    sign n = o if n >= 0 then "+" else "-"

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

refSwap :: (MonadRef m, r ~ Ref m) => Object Identity -> Identity (m (Object r))
refSwap = \case
  Symbol s -> pure $ pure $ Symbol s
  Character c -> pure $ pure $ Character c
  Stream r -> readRef r >>= \s -> pure $ fmap Stream $ newRef s
  Pair r -> readRef r >>= \case
    Number n -> pure $ fmap Pair $ newRef $ Number n
    Continuation c -> pure $ fmap Pair $ newRef $ Continuation c
    OptimizedFunction (MkOptimizedFunction body (fba, fbb)) -> do
      fba' <- refSwap fba
      fbb' <- refSwap fbb
      pure $ do
        fba'' <- fba'
        fbb'' <- fbb'
        fmap Pair $ newRef $ OptimizedFunction $
          MkOptimizedFunction body (fba'', fbb'')
    MkPair (car, cdr) -> do
      car' <- refSwap car
      cdr' <- refSwap cdr
      pure $ do
        car'' <- car'
        cdr'' <- cdr'
        car'' .* cdr''

properList1 :: (MonadMutableRef m, r ~ Ref m) => r (Pair r) -> m (Maybe (NonEmpty (Object r)))
properList1 ref = readPair "properList1" ref >>=
  \(car, cdr) -> fmap (car :|) <$> properList cdr

properList :: (MonadMutableRef m, r ~ Ref m) => Object r -> m (Maybe [Object r])
properList = \case
  Symbol Nil -> pure (Just [])
  Pair ref -> toList <$$> properList1 ref
  _ -> pure Nothing

properListOf
  :: (MonadMutableRef m, r ~ Ref m)
  => Object r -> (Object r -> MaybeT m a) -> MaybeT m [a]
properListOf x f = MaybeT (properList x) >>= traverse f

string :: (MonadRef m, r ~ Ref m) => Object r -> m (Maybe [Character])
string = runMaybeT . go where
  go = \case
    Symbol Nil -> pure []
    Pair ref -> readRef ref >>= \case
      MkPair (Character c, x) -> (c:) <$> go x
      _ -> empty
    _ -> empty

quote :: (MonadRef m, r ~ Ref m) => Object r -> m (Object r)
quote = ("quote" .|)

pattern Sym       :: Char -> String -> Object r
pattern Sym n ame = Symbol (MkSymbol (n :| ame))

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
      <|> maybeNumber
    go "(" ")" ref <&> \s -> maybe s id mb
    where
      maybeNumber :: MaybeT m String
      maybeNumber = number (Pair ref) <&> showNumber

      showNumber :: Complex Rational -> String
      showNumber (real :+ imag) =
        showRatio real False <> if imag == 0 then "" else (showRatio imag True <> "i")

      showRatio :: Rational -> Bool -> String
      showRatio r b = (if b && r >=0 then "+" else "")
        <> show (numerator r) <> if denominator r == 1 then "" else ("/" <> show (denominator r))

      maybeQuoted :: String -> String -> MaybeT m String
      maybeQuoted name p = readRef ref >>= \case
        MkPair (Sym n ame, Pair rest) | n:ame == name ->
           readRef rest >>= \case
             MkPair (x, Symbol Nil) -> repr x <&> (p <>)
             _ -> empty
        _ -> empty
      maybeQuoted' name p = readRef ref >>= \case
        MkPair (Sym n ame, x) | n:ame == name -> repr x <&> (p <>)
        _ -> empty
      escaped :: Character -> String
      escaped c = maybe (pure $ unCharacter c) ("\\" <>) (escapeSequence c)
      go :: String -> String -> r (Pair r) -> m String
      go pre post ref' = readRef ref' >>= \case
        Number n -> pure $ showNumber n
        Continuation _ -> pure "<continuation>"
        OptimizedFunction f -> (newRef $ MkPair $ fnFallback f) >>= repr
        MkPair (car, cdr) -> repr car >>= \car' ->
          (\s -> pre <> s <> post) . (car' <>) <$> mcase2 (number, id) cdr \case
            Case1of2 n -> pure (" . " <> showNumber n)
            Case2of2 (Symbol Nil) -> pure ""
            Case2of2 (Pair p') -> (" " <>) <$> go "" "" p'
            Case2of2 x -> (" . " <>) <$> repr x

instance Repr m Character where
  repr c = pure $ "\\" <> maybe (pure (unCharacter c)) id (escapeSequence c)
instance Repr m (Object m) where
  repr = \case
    Symbol s -> repr s
    Pair p -> repr p
    Character c -> repr c
    Stream _ -> pure "<stream>"

mcase2 :: Monad m => (x -> MaybeT m a, x -> b) -> x -> (Case2 a b -> m r) -> m r
mcase2 (a, b) x r = (runMaybeT (Case1of2 <$> a x)) >>= r . maybe (Case2of2 (b x)) id

mcase3 :: Monad m => (x -> MaybeT m a, x -> MaybeT m b, x -> c) -> x -> (Case3 a b c -> m r) -> m r
mcase3 (a, b, c) x r = flip bind r
  (runMaybeT (a x) >>= flip maybe (pure . Case1of3)
    (runMaybeT (b x) >>= flip maybe (pure . Case2of3)
      (pure $ Case3of3 $ c x)))

data Case2 a b
  = Case1of2 a
  | Case2of2 b

data Case3 a b c
  = Case1of3 a
  | Case2of3 b
  | Case3of3 c

pattern Nil :: Symbol
pattern Nil = MkSymbol ('n' :| "il")

number :: forall m r. (MonadRef m, r ~ Ref m) => Object r -> MaybeT m Number
number = \case
  Pair ref -> readRef ref >>= \case
    Number n -> pure n
    MkPair (Sym 'l' "it", Pair a) -> readRef a >>= \case
      MkPair (Sym 'n' "um", Pair b) -> readRef b >>= \case
        MkPair (real, Pair c) -> readRef c >>= \case
          MkPair (imag, Symbol Nil) ->
            bisequence (ratio real, ratio imag) <&> uncurry (:+)
          _ -> empty
        _ -> empty
      _ -> empty
    _ -> empty
  _ -> empty
  where
  ratio :: Object r -> MaybeT m Rational
  ratio = \case
    Pair a -> readRef a >>= \case
      MkPair (sign -> Just s, Pair b) -> readRef b >>= \case
        MkPair (num, Pair c) -> readRef c >>= \case
          MkPair (denom, Symbol Nil) ->
            bisequence (integer num, integer denom) <&> \(n, d) -> (s * n) % d
          _ -> empty
        _ -> empty
      _ -> empty
    _ -> empty
  integer :: Object r -> MaybeT m Integer
  -- @incomplete: this doesn't work outside the Int range
  integer = \case
    Symbol Nil -> pure 0
    Pair ref -> readRef ref >>= \case
      MkPair (Sym 't' "", x) -> (1+) <$> integer x
      _ -> empty
    _ -> empty
  sign = \case
    Sym '+' "" -> Just 1
    Sym '-' "" -> Just (-1)
    _ -> Nothing

escapeSequence :: Character -> Maybe String
escapeSequence =
  (\c -> fst <$> (find ((== c) . snd) controlChars)) .
  unCharacter

controlChars :: [(String, Char)]
controlChars =
  [ ("tab", '\t')
  , ("nul", '\NUL')
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
