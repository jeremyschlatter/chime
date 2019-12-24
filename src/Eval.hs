{-# LANGUAGE UndecidableInstances #-}
module Eval where

import BasePrelude as P hiding (evaluate, getEnv, head, tail, mask)
import Control.Lens.Combinators hiding (op)
import Control.Lens.Operators hiding ((<|))
import Control.Monad.Cont hiding (cont)
import Control.Monad.Except hiding (throwError)
import qualified Control.Monad.Except as E
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bitraversable
import qualified Data.ByteString as B
import Data.FileEmbed
import Data.List.NonEmpty as NE (nonEmpty, head, tail, reverse, (<|))
import Data.Text (singleton)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import System.Console.Haskeline
import System.Directory
import System.FilePath
import System.IO
import qualified Text.Megaparsec as M
import Text.Megaparsec.Error

import Common
import Data
import Parse (isEmptyLine, parse, parseMany, errorBundlePretty)
import qualified Parse

builtins :: EvalMonad ()
builtins = (globe <~) $ traverse
    ((\(s, x') -> x' >>= \x -> newRef $ MkPair $ (s, x)) . first (Symbol . sym')) $
  [ ("nil", sym "nil")
  , ("o", sym "o")
  , ("apply", sym "apply")
  , ("t", sym "t")
  , ("ins", sym "nil")
  , ("outs", sym "nil")
  , ("chars",
      let convert = flip B.foldl [] \acc -> (acc <>) . \w ->
              toObject . bool '0' '1' . testBit w <$> P.reverse [0..7]
      in listToObject $ flip fmap [0..127] \i ->
           chr i .* (
            listToObject $ convert $ encodeUtf8 $ singleton $ chr i :: EvalMonad (Object IORef)
          )
    )
  , ("vmark", Pair <$> use vmark)
  ] <> fmap (\p -> (p, "lit" ~~ "prim" ~| p))
    [ "id"
    , "join"
    , "car"
    , "cdr"
    , "type"
    , "xar"
    , "xdr"
    , "sym"
    , "nom"
    , "wrb"
    , "rdb"
    , "ops"
    , "cls"
    , "stat"
    , "err"
    ]
  where
    -- NOTE: sym and sym' are unsafe!
    --   They error if called on the empty string.
    --   Do not factor them out of this local scope.
    sym = pure . Symbol . sym'
    sym' :: String -> Symbol
    sym' = \case
      [] -> interpreterBug "called sym' with an empty string"
      x:xs -> MkSymbol (x :| xs)

nativeFns :: [(String, OptimizedFunction IORef)]
nativeFns = fmap (second \f -> f { fnBody = traverse evaluate >=> fnBody f })
  [ ("+",) $ numFnN $ foldr numAdd (0 :+ 0)
  , ("-",) $ numFnN \case
      [] -> (0 :+ 0)
      [a] -> (0 :+ 0) `numSub` a
      a:rest -> numSub a $ foldr numAdd (0 :+ 0) rest
  , ("*",) $ numFnN $ foldr numMul (1 :+ 0)
  , ("recip",) $ numFn1 $ \(x :+ y) -> let d = x*x + y*y in (x/d) :+ (-y/d)
  , ("odd",) $ numFn1 \case
      (n :+ 0) -> denominator n == 1 && odd (numerator n)
      _ -> False
  , ("even",) $ numFn1 \case
      (n :+ 0) -> denominator n == 1 && even (numerator n)
      _ -> False
  -- @incomplete: compare other things, too
  , (">",) $ numComp (>) (>=)
  , (">=",) $ numComp (>=) (>=)
  , ("<",) $ numComp (<) (<=)
  , ("<=",) $ numComp (<=) (<=)
  , ("=",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) let
      go :: Object IORef -> [Object IORef] -> EvalMonad Bool
      go a = \case
        [] -> pure $ True
        x:xs -> eq a x >>= bool (pure False) (go a xs)
      eq :: Object IORef -> Object IORef -> EvalMonad Bool
      eq x y = bisequence (runMaybeT $ number x, runMaybeT $ number y) >>= \case
        (Just n, Just m) -> pure $ n  == m
        (Nothing, Nothing) -> case (x, y) of
          (Symbol a, Symbol b) -> pure $ a == b
          (Character a, Character b) -> pure $ a == b
          (Stream a, Stream b) -> pure $ a == b
          (Pair a, Pair b) -> bisequence (readPair "=" a, readPair "=" b) >>=
            \((aa, ad), (ba, bd)) -> eq aa ba >>= bool (pure False) (eq ad bd)
          _ -> pure False
        _ -> pure False
      in fmap (bool (Symbol Nil) (Sym 't' "")) . \case
        a:b:cs -> go a (b:cs)
        _ -> pure True
  , ("cons",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) $ let
      go = \case
        [] -> pure $ Symbol Nil
        [x] -> pure x
        x:xs -> x ~~ go xs
      in go
  , ("append",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) $ let
      go :: [Object IORef] -> [Object IORef] -> EvalMonad (Object IORef)
      go accum = \case
        [] -> listToObject (pure <$> accum)
        [x] -> case accum of
          [] -> pure x
          xs -> properList x >>= \case
            Nothing -> (pure @EvalMonad <$> xs) ~~ x
            Just x' -> listToObject (pure <$> (xs <> x'))
        x : xs -> properList x >>= \case
          Nothing -> repr x >>= throwError . ("tried to append to a non-list: " <>)
          Just l -> go (accum <> l) xs
      in go []
  , ("nth",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) \case
      [] -> tooFewArguments
      [_] -> tooFewArguments
      _:_:_:_ -> tooManyArguments
      [mn, mxs] -> runMaybeT (number mn) >>= \case
        Just (((numerator &&& denominator) -> (n', 1)) :+ 0) | n' >= 0 -> let
          go n = \case
            Pair ref -> readPair "nth" ref >>= \(car, cdr) ->
              if n == 1
              then use locs >>= \case
                Just _:rest -> (locs .= rest) *> (Pair ref ~| Sym @IORef 'a' "")
                _ -> pure car
              else go (n - 1) cdr
            _ -> typecheckFailure
          in go n' mxs
        _ -> typecheckFailure
  , ("bitc",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) $ let
      nextByte :: Stream -> EvalMonad (Either (Object IORef) Word8)
      nextByte (MkStream h _ _ idx) =
        if idx == 7
        then liftIO (B.unpack <$> B.hGet h 1) <&> \case
          [] -> Left $ Sym 'e' "of"
          [x] -> Right x
          _ -> interpreterBug
            "Haskell's Data.ByteString.hGet returned more bytes than it promised"
        else Left <$> throwError "bitc called on byte-unaligned stream"
      readStream = \case
        Stream s -> readRef s >>= go B.empty
        _ -> typecheckFailure
      go bs s = nextByte s >>= either pure (go' s . flip B.cons bs)
      go' :: Stream -> B.ByteString -> EvalMonad (Object IORef)
      go' s bs =
        either
          (const $ go bs s)
          (pure . Character . MkCharacter . T.head)
          (decodeUtf8' bs)
      in \case
        [] -> evaluate (Sym 'i' "ns") >>= readStream
        [x] -> readStream x
        _ -> tooManyArguments
  , ("parsenum",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) $ let
      symT = \case
        (Sym 't' "") -> pure ()
        _ -> empty
      in \case
        [] -> tooFewArguments
        [_] -> tooFewArguments
        [cs, base] -> bisequence
          (unCharacter <$$$> string cs, runMaybeT (properListOf base symT)) >>= \case
            -- @incomplete: handle bases other than 10
            (Just s, Just (length -> n)) | n == 10 ->
              either (throwError . errorBundlePretty) id
                (M.parse (runIdentity . refSwap <$> (Parse.number <* M.eof)) "" s)
            _ -> typecheckFailure
        _:_:_:_ -> tooManyArguments
  , ("floor",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) \case
      [] -> tooFewArguments
      _:_:_ -> tooManyArguments
      [x] -> runMaybeT (number x) >>= \case
        Just (n :+ 0) -> toObject $ (((floor n % 1) :+ 0) :: Complex Rational)
        _ -> typecheckFailure
  , ("number",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) \case
      [n] -> (runMaybeT (number n)) <&> \case
        Just _ -> Sym 't' ""
        _ -> Symbol Nil
      _ -> throwError "wrong number of arguments"
  , ("prsimple",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) \case
      [x, s] -> toStream Out s >>= \case
        Just ref -> readRef ref >>= \case
          MkStream h _ _ 7 -> let
            -- @consider: prsimple in bel.bel does not always return nil
            -- maybe this should mirror that behavior more accurately?
            proceed = (repr x >>= liftIO . hPutStr h) $> Symbol Nil
            in runMaybeT (number x) >>= \mn -> case (mn, x) of
              (_, Symbol _) -> proceed
              (_, Character _) -> proceed
              (_, Stream _) -> proceed
              (Just _, _) -> proceed
              _ -> throwError "cannot-print"
          _ -> throwError $
            "Tried to call prnum after writing n bits to this stream, where n % 8 != 0\n"
              <> "That was probably a mistake, but either way that operation is not supported "
              <> "right now. Sorry."
        _ -> typecheckFailure
      _ -> throwError "wrong number of arguments"

  , ("time",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) \case
      [x] -> do
        start <- liftIO getCurrentTime
        result <- evaluate x
        end <- liftIO getCurrentTime
        liftIO $ putStrLn $ show $ diffUTCTime end start
        pure result
      _ -> throwError "time requires exactly one argument"
  ]

nativeMacros :: [(String, OptimizedFunction IORef)]
nativeMacros =
  [ ("or",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) $ let
      go = \case
        [] -> pure $ Symbol Nil
        x:xs -> evaluate x >>= \case
          Symbol Nil -> go xs
          x' -> pure x'
      in go
  , ("and",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) $ let
      go = \case
        [] -> pure $ Sym 't' ""
        [x] -> evaluate x
        x:xs -> evaluate x >>= \case
          Symbol Nil -> pure $ Symbol Nil
          _ -> go xs
      in go
  , ("fn",) $ flip MkOptimizedFunction (Symbol Nil, Symbol Nil) \case
      parms : b:ody -> use scope >>= \(s:|_) ->
        "lit" ~~ "clo" ~~ (listToObject @EvalMonad (pure . Pair <$> s)) ~~ parms ~|
          case ody of
            [] -> pure b
            _ -> "do" ~~ go (b:ody) where
              go = \case
                [] -> pure $ Symbol Nil
                x:xs -> x ~~ go xs
      _ -> tooFewArguments
  ]

withNativeFns :: forall m. (MonadMutableRef m, IORef ~ Ref m) => EvalState -> m EvalState
withNativeFns startState = foldM oneFn startState (nativeFns <> nativeMacros) where
  oneFn :: EvalState -> (String, OptimizedFunction IORef) -> m EvalState
  oneFn s (nm, f) = runMaybeT (envLookup' (sym nm) (_globe s)) >>= \case
    Nothing -> case nm of
      "time" -> (fmap Pair . newRef . OptimizedFunction) f >>= \x ->
        (mkPair (sym nm) x <&> \p -> (s & globe %~ (p:)))
      _ -> interpreterBug $ nm <> " was not present in the global state"
    Just (_, p) -> case p of
      Pair ref -> readPair "native fns" ref >>= \n ->
        writeRef ref (OptimizedFunction (f {fnFallback = n})) $> s
      _ -> interpreterBug $ "non-pair in global binding for " <> nm
  sym = \case
    [] -> interpreterBug "unexpected empty string"
    n:ame -> Sym n ame

numAdd :: Number -> Number -> Number
numAdd a b = (realPart a + realPart b) :+ (imagPart a + imagPart b)

numSub :: Number -> Number -> Number
numSub a b = (realPart a - realPart b) :+ (imagPart a - imagPart b)

numMul :: Number -> Number -> Number
numMul (a :+ b) (c :+ d) = (a * c - b * d) :+ (a * d + b * c)

numComp
  :: (Rational -> Rational -> Bool) -> (Rational -> Rational -> Bool) -> OptimizedFunction IORef
numComp r i = numFnN go where
  go = \case
    [] -> True
    [_] -> True
    (ar :+ ai) : b@(br :+ bi) : cs -> r ar br && i ai bi && go (b:cs)

-- @incomplete: fallback to the definition in bel.bel if inputs are not numbers
numFnN
  :: (ToObject EvalMonad IORef r) => ([Number] -> r) -> OptimizedFunction IORef
numFnN f = flip MkOptimizedFunction (Symbol Nil, Symbol Nil) $
  traverse (runMaybeT . number) >=> maybe
    (throwError "non-numbers were passed to an optimized numeric function")
    (toObject . f)
    . sequence

numFn1 :: (ToObject EvalMonad IORef r) => (Number -> r) -> OptimizedFunction IORef
numFn1 f = flip MkOptimizedFunction (Symbol Nil, Symbol Nil) \case
  [x] -> runMaybeT (number x) >>= maybe
    (throwError "non-number was passed to an optimized numeric function")
    (toObject . f)
  _ -> tooFewArguments

throwError :: String -> EvalMonad (Object IORef)
throwError s = listToObject [
  o "err", listToObject (pure . Character . MkCharacter <$> s)] >>= evaluate

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap p = \case
  [] -> Nothing
  x:xs -> maybe (findMap p xs) Just (p x)

envLookup'
  :: forall m. (MonadRef m, IORef ~ Ref m)
  => Object IORef -> Environment -> MaybeT m (IORef (Pair IORef), Object IORef)
envLookup' k kvs = MaybeT $ traverse doRead kvs <&> findMap \p' -> case (k, p') of
  (Symbol a, (p, MkPair (Symbol b, v))) | a == b -> Just (p, v)
  (Pair a, (p, MkPair (Pair b, v))) | a == b -> Just (p, v)
  _ -> Nothing
  where
    doRead :: IORef (Pair IORef) -> m (IORef (Pair IORef), Pair IORef)
    doRead r = (r,) <$> readRef r

envLookup :: Object IORef -> Environment -> MaybeT EvalMonad (Object IORef)
envLookup = post .: envLookup' where
  post :: MaybeT EvalMonad (IORef (Pair IORef), Object IORef) -> MaybeT EvalMonad (Object IORef)
  post = flip bind \(p, v) -> use locs >>= \case
    Just _:rest -> (locs .= rest) *> listToObject [pure $ Pair p, pure $ Sym 'd' ""]
    _ -> pure v

vref :: Object IORef -> EvalMonad (Object IORef)
vref s = do
  dyns' <- use dyns
  scope' <- use $ scope._Wrapped._1
  globe' <- use globe
  locs' <- use locs
  runMaybeT (envLookup s dyns' <|> envLookup s scope' <|> envLookup s globe') >>= flip maybe pure do
    case locs' of
      Just True:rest -> do
        locs .= rest
        ref <- newRef $ MkPair (s, Symbol Nil)
        globe .= ref:globe'
        Pair ref ~| "d"
      _ -> throwError . ("undefined variable: " <>) =<< repr s

toVariable :: Object IORef -> MaybeT EvalMonad (Object IORef)
toVariable = \case
  s@(Symbol _) -> pure $ s
  p@(Pair r) -> bisequence (readPair "toVar" r, use vmark) >>= \case
    ((Pair carRef, _), v) -> case carRef == v of
      True -> pure p
      _ -> empty
    _ -> empty
  _ -> empty

function :: [Object IORef] -> MaybeT EvalMonad Closure
function x = case x of
  [Sym 'l' "it", Sym 'c' "lo", env, params, body] -> do
    env' <- properListOf env \case
      Pair r -> pure r
      _ -> empty
    pure $ MkClosure env' params body
  _ -> empty

macro :: [Object IORef] -> MaybeT EvalMonad Closure
macro = \case
  [Sym 'l' "it", Sym 'm' "ac", m] -> MaybeT (properList m) >>= function
  _ -> empty

virfn :: [Object IORef] -> MaybeT EvalMonad Closure
virfn = \case
  Sym 'l' "it" : f : _ -> use globe >>= envLookup' (Sym 'v' "irfns") >>=
    flip properListOf env . snd >>= envLookup' f >>= MaybeT . properList . snd >>= function
  _ -> empty
  where
    env = \case Pair p -> pure p; _ -> empty

data Operator
  = Primitive Primitive
  | SpecialForm SpecialForm
  | Macro Closure
  | Closure Closure
  | Virfn Closure
  | TheContinuation (Object IORef -> EvalMonad (Object IORef))
  | TheOptimizedFunction (OptimizedFunction IORef)

data Closure = MkClosure Environment (Object IORef) (Object IORef)

data ListExpr
  = StringExpr

data SpecialForm
  = Form1 String (Object IORef -> EvalMonad (Object IORef))
  | Form2 String (Object IORef -> Object IORef -> EvalMonad (Object IORef))
  | Form3 String (Object IORef -> Object IORef -> Object IORef -> EvalMonad (Object IORef))
  | FormN String ([Object IORef] -> EvalMonad (Object IORef))
  | Lit

formName :: SpecialForm -> String
formName = \case
  Form1 s _ -> s
  Form2 s _ -> s
  Form3 s _ -> s
  FormN s _ -> s
  Lit -> "lit"

specialForm :: Object IORef -> Maybe SpecialForm
specialForm = \case
  Symbol s -> lookup (toList $ unSymbol s) specialForms
  _ -> Nothing

pushBinding :: MonadState s EvalMonad =>
  ASetter s s Environment Environment -> Object IORef -> Object IORef -> EvalMonad ()
pushBinding env var val = ((:) <$> mkPair var val) >>= (env %=)

specialForms :: [(String, SpecialForm)]
specialForms = (\f -> (formName f, f)) <$>
  [ Form1 "quote" pure
  , Lit
  , FormN "if" let
      go = \case
        [] -> pure $ Symbol Nil
        [x] -> evreturn x
        b:x:rest -> evaluate b >>= \case
          Symbol Nil -> go rest
          _ -> evreturn x
      in go
  , FormN "apply" let
      go acc = \case
        x :| y:ys -> evaluate x >>= flip go (y:|ys) . (<|acc)
        -- @incomplete apply can take a dotted list in its last
        -- argument under some conditions, but I don't understand
        -- those conditions yet.
        ls :| [] -> evaluate ls >>= properList >>= \case
          Nothing -> throwError $
            "The last argument to apply must be a proper list, but was not. "
            <> "(apply should accept non-proper lists in some cases, but "
            <> "that has not been implemented yet)."
          Just lst -> go' acc lst where
            go' acc' = \case
              [] -> listToObject (pure (head ll) : fmap quote (tail ll)) >>= evreturn
                where ll = NE.reverse acc'
              c:cs -> go' (c<|acc') cs
      badParams = throwError "apply requires at least two parameters"
      in \case fa : r:est -> go (pure fa) (r:|est); _ -> badParams where
  , Form1 "where" $ formWhere False
  , Form3 "dyn" \v x y -> runMaybeT (toVariable v) >>= \case
        Just v' -> evaluate x >>= \evX -> pushDyn evX *> evreturn y <* popDyn where
          pushDyn = pushBinding dyns v'
          popDyn = dyns %= \case
            [] -> error "interpreter bug: should be impossible because of call to pushDyn"
            _:xs -> xs
        _ -> repr v >>= \rep -> throwError $ "dyn requires a variable as its first argument. "
          <> rep <> " is not a variable."
  , Form2 "after" \x y -> do
      x' <- catchError (Right <$> evreturn x) (pure . Left)
      _ <- evaluate y
      either E.throwError pure x'
  , Form1 "ccc" $ evaluate >=> \f -> callCC \cont -> do
      c <- Pair <$> (newRef @EvalMonad $ Continuation cont)
      listToObject [pure f, pure c] >>= evreturn
  , FormN "set" let
      go r = \case
        [] -> pure r
        [_] -> throwError "Odd number of arguments to set."

        -- Quietly ignore attempts to overwrite vmark.
        Sym 'v' "mark":_:rest -> Pair <$> use vmark >>= flip go rest

        var:val:rest -> evaluate val >>= \val' -> runMaybeT (toVariable var) >>= \case
          -- _ -> ("where" ~| var) >>= evaluate >>= properList >>= \case
          _ -> formWhere True var >>= \ttt -> properList ttt >>= \case
            Just
             [ Pair ref
             , Sym ((\case 'a' -> Just True; 'd' -> Just False; _ -> Nothing) -> Just setCar) ""
             ] -> readPair "set" ref >>= \(car, cdr) -> do
                writeRef ref $ MkPair $ bool (car, val') (val', cdr) setCar
                go val' rest
            _ -> repr ttt >>= throwError . ("Tried to use set on something that was neither a variable nor a pair: " <>)
      in go $ Symbol Nil
  -- @incomplete: this is just an approximation, since I haven't learned
  -- yet what the full scope of def is.
  , FormN "def" \case
      [] -> throwError "'def' received no arguments"
      -- @incomplete: does not capture scope in the three-argument case
      [n, p, e] -> evreturn =<<
        ("set" ~~ n ~| ("lit" ~~ "clo" ~~ "nil" ~~ p ~| e))
      n:rest -> evreturn =<< "set" ~~ n ~| ("fn" ~~ listToObject @EvalMonad (pure <$> rest))

  -- @incomplete: implement this in a way that cannot clash with user symbols
  , Form1 "~backquote" let
      go = \case
        Pair ref -> readPair "backquote" ref >>= \p -> case p of
          (Sym '~' "comma", x) -> Left <$> evaluate x
          (Sym '~' "splice", x) -> Right <$> evaluate x
          _ -> bimapM go go p >>= \(x, either id id -> cdr) -> Left <$> case x of
            Left car -> car .* cdr
            Right prefix -> append prefix cdr
        x -> pure $ Left x
      in fmap (either id id) . go

  , Form3 "mac" \n p e -> evreturn =<<
      ("set" ~~ n ~| ("lit" ~~ "mac" ~| ("lit" ~~ "clo" ~~ "nil" ~~ p ~| e)))

  ]

formWhere :: Bool -> Object IORef -> EvalMonad (Object IORef)
formWhere b x = do
  locs %= (Just b:)
  r <- evreturn x
  use locs >>= \case
    Just _:rest -> (locs .= rest) *>
      throwError "called where on a value that does not come from a pair"
    _ -> pure r

-- Specialize (.*) and (.|) to the EvalMonad, to avoid type ambiguities.
(~~) :: forall a b. (ToObject EvalMonad IORef a, ToObject EvalMonad IORef b)
     => a -> b -> EvalMonad (Object IORef)
(~~) = (.*)
infixr 4 ~~
(~|) :: forall a b. (ToObject EvalMonad IORef a, ToObject EvalMonad IORef b)
     => a -> b -> EvalMonad (Object IORef)
(~|) = (.|)
infixr 4 ~|


append :: Object IORef -> Object IORef -> EvalMonad (Object IORef)
append a b = bimapM properList properList (a, b) >>= \case
  (Just a', Just b') -> listToObject . fmap pure $ a' <> b'
  _ -> throwError "non-lists in splice"

data Primitive
  = Prim1 String (Object IORef -> EvalMonad (Object IORef))
  | Prim2 String (Object IORef -> Object IORef -> EvalMonad (Object IORef))

primName :: Primitive -> String
primName = \case
  Prim1 s _ -> s
  Prim2 s _ -> s

primitive :: [Object IORef] -> Maybe Primitive
primitive = \case
  [Sym 'l' "it", Sym 'p' "rim", Symbol x] -> lookup (toList $ unSymbol x) primitives
  _ -> Nothing

primitives :: [(String, Primitive)]
primitives = (\p -> (primName p, p)) <$>
  [ Prim2 "id" $ pure . fromBool .: curry \case
      (Symbol a, Symbol b) -> a == b
      (Character a, Character b) -> a == b
      (Stream a, Stream b) -> a == b
      (Pair a, Pair b) -> a == b
      _ -> False
  , Prim2 "join" (.*)
  , carAndCdr "car" fst 'a'
  , carAndCdr "cdr" snd 'd'
  , Prim1 "type" $ let
      go = \case
        Symbol _ -> 's' :| "ymbol"
        Character _ -> 'c' :| "har"
        Pair _ -> 'p' :| "air"
        Stream _ -> 's' :| "tream"
      in pure . Symbol . MkSymbol . go
  , xarAndXdr "xar" first
  , xarAndXdr "xdr" second
  , Prim1 "sym" \x -> string x >>= \s' -> case s' >>= nonEmpty of
      Just s -> pure $ Symbol $ MkSymbol $ fmap unCharacter s
      Nothing -> repr x >>= \rep -> throwError $ "sym is only defined on non-empty strings. "
        <> rep <> " is not a non-empty string."
  , Prim1 "nom" \case
      Sym n ame -> listToObject (toObject <$> n:ame)
      x -> repr x >>= \rep -> throwError $ "nom is only defined on symbols. "
        <> rep <> " is not a symbol."
  , Prim2 "wrb" \b' s' -> do
      let wBit = case b' of
            Character (MkCharacter '0') -> Just False
            Character (MkCharacter '1') -> Just True
            _ -> Nothing
      stream <- runMaybeT @EvalMonad $ case s' of
        Stream r -> (r,) <$> readRef r
        Symbol Nil -> use outs >>= \r -> (r,) <$> readRef r
        _ -> empty
      case (wBit, stream) of
        (Just b, Just (ref, MkStream h d buf idx)) -> let
          newBuf = if b then setBit buf idx else buf
          newIdx = (idx + 7) `mod` 8
          in Symbol Nil <$
            if newIdx == 7
            then do
              liftIO $ B.hPut h (B.singleton newBuf)
              writeRef ref $ MkStream h d 0 newIdx
            else
              writeRef ref $ MkStream h d newBuf newIdx
        _ -> throwError "invalid arguments to wrb"
  , Prim1 "rdb" let
      -- rdb' = (flip bind (\x -> repr x >>= \r -> traceM r $> x)) . rdb
      rdb ref = readRef ref >>= \(MkStream h d buf idx) ->
        let readBit x = Character $ MkCharacter $ bool '0' '1' $ testBit x idx
        in if idx == 7
           then do
             -- @incomplete: this blocks, and rdb should not block
             -- Fixing this will be difficult because the underlying Haskell interface
             -- does not support a non-blocking read that also reports EOF.
             b <- liftIO $ B.hGet h 1
             case B.unpack b of
               [] -> pure $ Sym 'e' "of"
               -- [] -> pure $ Symbol Nil
               [x] -> writeRef ref (MkStream h d x 6) $> readBit x
               _ -> interpreterBug
                 "Haskell's Data.ByteString.hGet returned more bytes than it promised"
           else writeRef ref (MkStream h d buf ((idx + 7) `mod` 8)) $> readBit buf
      in \case
        Stream ref -> rdb ref
        Symbol Nil -> use ins >>= rdb
        x -> repr x >>= throwError . ("tried to read from a non-stream: " <>)
  , Prim2 "ops" \x y -> string x >>= \x' -> case (x', y) of
      (
        Just path,
        Symbol (
          (\s -> case toList $ unSymbol s of
                    "in" -> Just (ReadMode, In)
                    "out" -> Just (WriteMode, Out)
                    _ -> Nothing
          ) -> Just (mode, dir))
       -- @incomplete: catch IO errors and lift them to Bel errors
       ) -> lift $ lift $ lift $ openFile (unCharacter <$> path) mode >>= \h ->
         Stream <$> newStream dir h
      _ -> throwError "invalid arguments to ops"
  , Prim1 "cls" \case
      Stream r -> readRef r >>= \(MkStream h d b m) -> lift $ lift $ lift do
        if m == 7 || d == In
        then pure ()
        -- flush any bits left in the buffer
        else B.hPut h (B.singleton b)
        hClose h
        pure $ Symbol Nil
      _ -> throwError "invalid argument to cls"
  -- @incomplete: this throws away info when the argument is not a string
  -- @consider: what if another error is signaled during evaluation of the argument?
  , Prim1 "err" $ \x -> repr x >>= \rep -> string x >>= E.throwError . \case
      Nothing -> rep
      Just s -> unCharacter <$> s
  ] where
      carAndCdr nm fn w = Prim1 nm \case
        Symbol Nil -> pure $ Symbol Nil
        -- If we are inside a "where", return the tuple and our location.
        -- Otherwise, we return the normal value.
        Pair ra -> readPair "car/cdr" ra >>= \tup -> use locs >>= \case
          Just _:rest -> (locs .= rest) *> (Pair ra ~| Sym @IORef w "")
          _ -> pure $ fn tup
        x -> repr x >>= \s -> throwError $ nm
          <> " is only defined on pairs and nil. " <> s <> " is neither of those."
      xarAndXdr nm which = Prim2 nm $ curry \case
        (Pair r, y) -> ((readPair "xar/xdr" r <&> MkPair . (which $ const y)) >>= writeRef r) $> y
        (x, _) -> repr x >>= \s -> throwError $ nm
          <> " is only defined when the first argument is a pair. "
          <> s <> " is not a pair."

toStream :: Direction -> Object IORef -> EvalMonad (Maybe (IORef Stream))
toStream = runMaybeT .: curry \case
  (In, Symbol Nil) -> use ins
  (Out, Symbol Nil) -> use outs
  (_, Stream s) -> pure s
  _ -> empty

operator :: Object IORef -> EvalMonad (Maybe Operator)
operator = \case
  (specialForm -> Just f) -> pure $ Just $ SpecialForm f
  x -> evaluate x >>= \case
    Pair ref -> readRef ref >>= \case
      Continuation c -> pure $ Just $ TheContinuation c
      OptimizedFunction f -> pure $ Just $ TheOptimizedFunction f
      Number _ -> Virfn <$$> runMaybeT (virfn [Sym 'l' "it", Sym 'n' "um"])
      _ -> properList1 ref >>= \case
        Just (primitive . toList -> Just f) -> pure $ Just $ Primitive f
        Just (toList -> l) -> runMaybeT $
          (Closure <$> function l) <|> (Macro <$> macro l) <|> (Virfn <$> virfn l)
        _ -> pure Nothing
    _ -> pure Nothing

toOptionalVar :: Object IORef -> EvalMonad (Maybe (Object IORef, Object IORef))
toOptionalVar p = properList p <&> \case
  Just [Sym 'o' "", x] -> Just (x, Symbol Nil)
  Just [Sym 'o' "", x, y] -> Just (x, y)
  _ -> Nothing

toTypeCheck :: Object IORef -> MaybeT EvalMonad (Object IORef, Object IORef)
toTypeCheck x = MaybeT (properList x) >>= \case
  [Sym 't' "", v, f] -> pure (v, f)
  _ -> empty

destructure :: Object IORef -> Object IORef -> EvalMonad (Either (Object IORef) Environment)
destructure p a' = pushScope *> go p a' <* popScope where
  pushScope = scope %= \(s:|ss) -> s:|(s:ss)
  popScope = scope %= \case
    _:|s:ss -> s:|ss
    _ -> interpreterBug "failed to popScope"
  pushVar v a = mkPair v a >>= \b -> (scope %= \(s:|ss) -> (b:s) :| ss) $> Right [b]
  go paramTree arg = mcase3 (toVariable, toTypeCheck, id) paramTree \case
    Case1of3 (Symbol Nil) -> case arg of
      Symbol Nil -> pure $ Right []
      _ -> Left <$> tooManyArguments
    Case1of3 v -> pushVar v arg -- pure $ Right [(v, arg)]
    Case2of3 (v, f) -> listToObject [pure f, quote arg] >>= evaluate >>= \case
      Symbol Nil -> Left <$> typecheckFailure
      _ -> go v arg
    -- @incomplete: Show more information about the function
    Case3of3 (Character _) -> fmap Left $ throwError $
      "Invalid function definition. The parameter definition must "
      <> "consist entirely of variables, but this one contained a character."
    Case3of3 (Stream _) -> fmap Left $ throwError $
      "Invalid function definition. The parameter definition must "
      <> "consist entirely of variables, but this one contained a stream."
    Case3of3 (Symbol _) -> interpreterBug "I mistakenly thought `toVariable` caught all symbols"
    Case3of3 (Pair pRef) ->
      -- let go' (pf1, a1) (pf2, a2) = bisequence (go pf1 a1, go pf2 a2) <&> uncurry (liftA2 (<>))
      let go' (pf1, a1) (pf2, a2) = (liftA2 (<>)) <$> go pf1 a1 <*> go pf2 a2
      in readPair "destructure 1" pRef >>= \(p1, p2) ->
        bisequence (toOptionalVar p1, toOptionalVar p2) >>= \(o1, o2) ->
          case arg of
            Pair aRef -> do
              let toVar v = maybe v fst
              (a1, a2) <- readPair "destructure 2" aRef
              go' (toVar p1 o1, a1) (toVar p2 o2, a2)
            x -> case (o1, o2) of
              (Nothing, Nothing) -> Left <$> tooFewArguments
              (Nothing, Just (v2, d2)) -> evaluate d2 >>= \e2 -> go' (p1, x) (v2, e2)
              -- @consider: is this behavior correct? these cases feel weird
              -- ((fn ((o x) . y) t))
              (Just (v1, d1), Nothing) -> evaluate d1 >>= \e1 -> go' (v1, e1) (p2, x)
              -- ((fn ((o x) . (o y)) t))
              (Just (v1, _d1), Just (v2, d2)) -> evaluate d2 >>= \e2 ->
                go' (v1, x) (v2, e2)

tooFewArguments :: EvalMonad (Object IORef)
tooFewArguments = throwError "Too few arguments in function call"

tooManyArguments :: EvalMonad (Object IORef)
tooManyArguments = throwError "Too many arguments in function call"

typecheckFailure :: EvalMonad (Object IORef)
typecheckFailure = throwError "typecheck failure"

with :: MonadState s m => ASetter s s [e] [e] -> m a -> e -> m a
with l m e = push *> m <* pop where
  push = l %= (e:)
  pop = l %= \case
    [] -> interpreterBug "failed to pop stack frame"
    _:xs -> xs

evaluate :: Object IORef -> EvalMonad (Object IORef)
evaluate = flip (with locs) Nothing . evreturn

evreturn :: Object IORef -> EvalMonad (Object IORef)
evreturn expr = {-bind (repr expr) $ with debug $-} case expr of
  -- characters
  c@(Character _) -> pure c

  -- streams
  s@(Stream _) -> pure s

  -- built-in symbols
  (Sym 't' "") -> pure expr
  (Sym 'o' "") -> pure expr
  (Sym 'a' "pply") -> pure expr
  (Sym 'n' "il") -> pure expr

  -- symbols
  (Symbol (MkSymbol (toList -> s'))) -> case s' of
    "globe" -> getEnv globe
    "scope" -> getEnv $ scope._Wrapped._1
    _ -> vref expr
    where getEnv = use >=> listToObject . fmap (pure . Pair)

  -- pairs
  Pair ref -> readRef ref >>= \case
    Number _ -> pure expr
    Continuation _ -> pure expr
    _ -> (,)
      <$> runMaybeT (toVariable expr)
      <*> properList1 ref >>= \case

      -- vmark references
      (Just _, _) -> vref expr

      (_, Just l@(op :| args)) -> case all isChar l of
        -- strings
        True -> pure expr

        -- operators with arguments
        False -> operator op >>= maybe giveUp \case
          TheContinuation c -> case args of
            [] -> throwError "tried to call a continuation with no arguments"
            [x] -> evaluate x >>= c
            _ -> throwError "tried to call a continuation with too many arguments"
          TheOptimizedFunction f -> fnBody f args
          Primitive p -> case p of
            Prim1 nm f -> case args of
              [] -> call (Symbol Nil)
              [a] -> call a
              _ -> excessPrimParams nm args 1
              where call = evaluate >=> f
            Prim2 nm f -> case args of
              [] -> call (Symbol Nil) (Symbol Nil)
              [a] -> call a (Symbol Nil)
              [a,b] -> call a b
              _ -> excessPrimParams nm args 2
              where
                call a b = do
                  a' <- evaluate a
                  b' <- evaluate b
                  f a' b'
          SpecialForm form -> case form of
            Form1 nm f -> case args of [a] -> f a; _ -> wrongParamCount nm args 1
            Form2 nm f -> case args of [a,b] -> f a b; _ -> wrongParamCount nm args 2
            Form3 nm f -> case args of [a,b,c] -> f a b c; _ -> wrongParamCount nm args 3
            FormN _ f -> f args
            Lit -> pure expr
          Closure (MkClosure env params body) ->
            (traverse evaluate >=> listToObject . fmap pure >=> destructure params) args >>=
              either pure
                (\bound -> withScope (bound <> env) (evreturn body))
          Macro (MkClosure env params body) -> do
            (_, argTree) <- readPair "macro args" ref
            destructure params argTree >>=
              either pure
                (\bound -> (withScope (bound <> env) (evaluate body)) >>= evreturn)
          Virfn (MkClosure env params body) -> do
            (_, argTree) <- readPair "virfn args" ref
            (op ~| argTree) >>= destructure params >>=
              either pure
                (\bound -> (withScope (bound <> env) (evaluate body)) >>= evreturn)

      _ -> giveUp

  where
    isChar = \case Character _ -> True; _ -> False
    withScope :: Environment -> EvalMonad a -> EvalMonad a
    withScope env a = push *> a <* pop where
      push = scope %= (env <|)
      pop = scope %= \case
        _:|[] -> interpreterBug "can't pop scope"
        _:|x:xs -> x:|xs
    giveUp = repr expr >>=
      throwError . ("I don't know how to evaluate this yet: " <>)

excessPrimParams :: String -> [a] -> Int -> EvalMonad (Object IORef)
excessPrimParams nm args n =
  throwError $ "Too many parameters in call to primitive " <> nm
    <> ". Got " <> show (length args) <> ", want at most " <> show n <> "."

wrongParamCount :: String -> [a] -> Int -> EvalMonad (Object IORef)
wrongParamCount nm args n =
  throwError $ "Wrong number of parameters in special form " <> nm
    <> ". Got " <> show (length args) <> ", want exactly " <> show n <> "."

fromBool :: Bool -> Object r
fromBool = \case
  True -> Sym 't' ""
  False -> Symbol Nil

runEval :: EvalMonad (Object IORef) -> EvalState -> IO (Either Error (Object IORef), EvalState)
runEval = runStateT . flip runContT pure . runExceptT

readThenEval :: FilePath -> String -> EvalMonad (Object IORef)
readThenEval path =
  (>>= evaluate) .
  either (throwError . errorBundlePretty) id .
  parse path

readThenRunEval :: FilePath -> String -> EvalState -> IO (Either Error (Object IORef), EvalState)
readThenRunEval p c s = flip runEval s $ readThenEval p c

builtinsIO :: IO EvalState
builtinsIO = snd <$> (runEval (builtins $> Symbol Nil) =<< emptyState)

bel :: FilePath -> IO (Either Error EvalState)
bel f = preludeIO >>= \b -> readFile f >>= \s0 -> do
  prog <- either (die . errorBundlePretty) id (parseMany f s0)
  (x, s) <- runEval (traverse_ evaluate prog $> Symbol Nil) b
  pure (x $> s)

getOrCreateHistoryFile :: IO FilePath
getOrCreateHistoryFile = do
  dir <- getXdgDirectory XdgCache "bel"
  createDirectoryIfMissing True dir
  pure $ dir </> "bel-repl-history.txt"

preludeIO :: IO EvalState
preludeIO = do
  b <- builtinsIO
  prog <- either (die . errorBundlePretty) id (parseMany "bel.bel" belDotBel)
  (x, s) <- runEval (traverse_ evaluate prog $> Symbol Nil) b
  either
    (\e -> interpreterBug $ "failed to interpret bel.bel: " <> e)
    withNativeFns
    (x $> s)

red :: String -> String
red s = "\ESC[31m" <> s <> "\ESC[0m"

repl :: IO ()
repl = getArgs >>= \case
  -- [f] -> putStrLn "hi"
  [f] -> bel f >>= either die (const (pure ()))
  _:_:_ -> die "Sorry, I can only handle up to one file"
  [] -> do
    s <- preludeIO
    hist <- getOrCreateHistoryFile
    runInputT ((defaultSettings @IO)
      { complete = noCompletion
      , historyFile = Just hist
      }) $ withInterrupt $ go "" s where
    go :: String -> EvalState -> InputT IO ()
    go prefix s = getExtendedInput prefix >>= \case
      Nothing -> pure ()
      Just input -> if isEmptyLine input then newline *> go "" s else do
        case parse @EvalMonad "input" input of
          Left err -> if isUnexpectedEOF err
                      then go (input <> "\n") s
                      else outputStrLn (red (errorBundlePretty err)) *> go "" s
          Right obj -> handleInterrupt (newline *> newline *> go "" s) do
            (x, s') <- lift $ runEval (obj >>= evaluate) s
            either (pure . red) repr x >>= outputStrLn
            newline
            go "" $ either (const s) (const s') x
    newline = outputStrLn "" -- empty line between inputs
    getExtendedInput :: String -> InputT IO (Maybe String)
    getExtendedInput prefix = handleInterrupt (newline *> getExtendedInput "")
      ((prefix <>) <$$> getInputLine (if prefix == "" then "> " else "| "))
    isUnexpectedEOF :: ParseErrorBundle String Void -> Bool
    isUnexpectedEOF b = case toList (bundleErrors b) of
      [TrivialError _ (Just EndOfInput) _] -> True
      _ -> False

belDotBel :: String
belDotBel = $(embedStringFile "reference/bel.bel")
