{-# LANGUAGE UndecidableInstances #-}
module Data where

import Control.Lens.Combinators (makeLenses, makePrisms)
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bitraversable
import qualified Data.ByteString as B
import Data.Text (pack)
import Data.Text.Encoding
import System.IO
import System.Random

import Common

-- -----------------------------------------------------------------------

type Number = Complex Rational

newtype Symbol = MkSymbol { unSymbol :: NonEmpty Char } deriving (Eq, Ord)

newtype Character = MkCharacter { unCharacter :: Char } deriving Eq

data Pair r
  = MkPair (Object r, Object r)
  | Number Number
  | Continuation (Object IORef -> EvalMonad (Object IORef))

data Direction = In | Out deriving Eq

class StreamBackend x where
  hGet :: x -> Int -> IO B.ByteString
  hPut :: x -> B.ByteString -> IO ()
  hClose :: x -> IO ()

instance StreamBackend Handle where
  hGet = B.hGet
  hPut = B.hPut
  hClose = System.IO.hClose

hPutStr :: StreamBackend x => x -> String -> IO ()
hPutStr h = hPut h . encodeUtf8 . pack

instance StreamBackend (IORef B.ByteString) where
  hGet h n = readIORef h >>= \s -> writeIORef h (B.drop n s) $> B.take n s
  hPut h s = modifyIORef h (`B.append` s)
  hClose _ = pure ()

data Stream where
  MkStream :: StreamBackend x =>
    { streamHandle :: x
    , streamDirection :: Direction
    , streamBuf :: Word8
    , streamPlace :: Int
    } -> Stream

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
 , _stack :: [Object IORef]
 , _doDebug :: Bool
 , _vmark :: IORef (Pair IORef)
 , _ins :: IORef Stream
 , _outs :: IORef Stream
 , _rng :: StdGen
 }
$(makeLenses ''EvalState)
$(makePrisms ''Object)

newStream :: (MonadRef m, Ref m ~ IORef, StreamBackend x) => Direction -> x -> m (IORef Stream)
newStream d h = newRef (MkStream h d 0 7)

emptyState :: (MonadRef m, Ref m ~ IORef, MonadIO m) => m EvalState
emptyState = EvalState [] (pure []) [] [] [] False
  <$> newRef (MkPair (Symbol Nil, Symbol Nil))
  <*> newStream In stdin
  <*> newStream Out stdout
  <*> liftIO newStdGen


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

instance EqRef r => Repr r (Either (r (Pair r)) Symbol) where
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
  toObject n = map Pair $ newRef $ Number n
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

pureListToObject :: forall m r. (MonadRef m, r ~ Ref m) => [Object r] -> m (Object r)
pureListToObject = \case
  [] -> pure $ Symbol Nil
  x:xs -> x .* pureListToObject @m xs

(.*) :: forall m r a b. (MonadRef m, r ~ Ref m, ToObject m r a, ToObject m r b)
     => a -> b -> m (Object r)
(.*) = map Pair . ((newRef . MkPair) =<<) . bimapM (toObject @m) (toObject @m) .: (,)
infixr 4 .*
(.|) :: forall m r a b. (MonadRef m, r ~ Ref m, ToObject m r a, ToObject m r b)
     => a -> b -> m (Object r)
a .| b = a .* ((.*) @m b "nil") where
infixr 4 .|

(><) :: MonadRef m => m (Object (Ref m)) -> m (Object (Ref m)) -> m (Object (Ref m))
(><) = (.*)
infixr 4 ><

properList1 :: (MonadMutableRef m, r ~ Ref m) => r (Pair r) -> m (Maybe (NonEmpty (Object r)))
properList1 ref = readPair "properList1" ref >>=
  \(car, cdr) -> map (car :|) <$> properList cdr

properList :: (MonadMutableRef m, r ~ Ref m) => Object r -> m (Maybe [Object r])
properList = \case
  Symbol Nil -> pure (Just [])
  Pair ref -> toList <$$> properList1 ref
  _ -> pure Nothing

properListOf
  :: (MonadMutableRef m, r ~ Ref m)
  => (Object r -> MaybeT m a) -> Object r -> MaybeT m [a]
properListOf f x = MaybeT (properList x) >>= traverse f

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

type Shares r = [(r (Pair r), (Int, Bool))]

type EqRef r = forall a. Eq (r a)

collectPairs :: forall m r. (MonadRef m, r ~ Ref m, EqRef r) => r (Pair r) -> m (Shares r)
collectPairs = map fst . go ([], []) . Pair where
  go :: (Shares r, [r (Pair r)]) -> Object r -> m (Shares r, [r (Pair r)])
  go (shares, seen) = \case
    Pair ref ->
      let (stop, shares', seen') = bool
            (False, shares, ref:seen)
            (True, (maybe
               ((ref, (length shares + 1, False)):shares)
               (const shares)
               (lookup ref shares))
             , seen)
            (elem ref seen)
      in if stop then pure (shares', seen') else readRef ref >>= \case
        MkPair (car, cdr) -> go (shares', seen') car >>= flip go cdr
        _ -> pure (shares', seen')
    _ -> pure (shares, seen)

class Repr r x where
  repr :: (MonadRef m, r ~ Ref m) => x -> m String
  reprShare :: (MonadRef m, r ~ Ref m) => x -> StateT (Shares r) m String
  reprShare = lift . repr
instance Repr r Symbol where
  repr x = let
    s = toList $ unSymbol $ x
    -- @incomplete: This is not all of the cases in which
    --   the vertical bar escape is required.
    --   This also does not escape vertical bars embedded in
    --   the symbol.
    r = if elem ' ' s then "¦" <> s <> "¦" else s
    in pure r
instance EqRef r' => Repr r' (r' (Pair r')) where
  repr :: (EqRef r, MonadRef m, r ~ Ref m) => r (Pair r) -> m String
  repr ref = collectPairs ref >>= evalStateT (reprShare ref)
  reprShare
    :: forall r m. (EqRef r, MonadRef m, r ~ Ref m)
    => r (Pair r) -> StateT (Shares r) m String
  reprShare ref = do
    mb <- lift $ runMaybeT $
      (MaybeT (string (Pair ref) <&&> \l -> "\"" <> foldMap escaped l <> "\""))
      <|> maybeNumber
    go "(" ")" ref <&> \s -> maybe s id mb
    where
      maybeNumber :: MaybeT m String
      maybeNumber = number (Pair ref) <&> showNumber

      escaped :: Character -> String
      escaped (MkCharacter ' ') = " " -- special case for space, not escaped when embedded in string
      escaped c = maybe (pure $ unCharacter c) ("\\" <>) (escapeSequence c)

      setB rr = \case
        [] -> []
        (r,(ri,_)):xs | r == rr -> (r, (ri, True)) : xs
        x:xs -> x : setB rr xs

      go :: String -> String -> r (Pair r) -> StateT (Shares r) m String
      go pre post ref' = do
        shares <- get
        case lookup ref' shares of
          Just (i, True) -> pure $ '#' : show i
          Just (i, False) -> do
            put $ setB ref' shares
            rep ('#' : show i <> "=")
          Nothing -> rep ""
        where
          rep ms =
            readRef ref' >>= \case
              Number n -> pure $ ms <> showNumber n
              Continuation _ -> pure $ ms <> "<continuation>"
              MkPair (car, cdr) -> reprShare car >>= \car' ->
                (\s -> (ms <> pre <> s <> post)) . (car' <>)
                  <$> mcase2 (number, id) cdr \case
                    Case1of2 n -> pure (" . " <> showNumber n)
                    Case2of2 (Symbol Nil) -> pure ""
                    Case2of2 (Pair p') -> get >>= \st' -> case lookup p' st' of
                      Just (i, True) -> pure $ " . #" <> show i
                      Just _ -> (" . " <>) <$> go "(" ")" p'
                      _ -> (" " <>) <$> go "" "" p'
                    Case2of2 x -> (" . " <>) <$> reprShare x

instance Repr r Character where
  repr c = pure $ "\\" <> maybe (pure (unCharacter c)) id (escapeSequence c)
instance EqRef r => Repr r (Object r) where
  repr = \case
    Symbol s -> repr s
    Pair p -> repr p
    Character c -> repr c
    Stream _ -> pure "<stream>"
  reprShare = \case
    Symbol s -> reprShare s
    Pair p -> reprShare p
    Character c -> reprShare c
    Stream _ -> pure "<stream>"

showNumber :: Complex Rational -> String
showNumber (real :+ imag) =
  showRatio real False <> if imag == 0 then "" else (showRatio imag True <> "i")
  where
  showRatio :: Rational -> Bool -> String
  showRatio r b = (if b && r >=0 then "+" else "")
    <> show (numerator r) <> if denominator r == 1 then "" else ("/" <> show (denominator r))

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
  , ("\"", '"')
  ]
