{-# LANGUAGE UndecidableInstances #-}
module Eval where

import BasePrelude hiding (evaluate, getEnv, head, tail, (-), (*))
import Control.Lens.Combinators hiding (op)
import Control.Lens.Operators hiding ((<|))
import Control.Monad.Except
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State hiding (get, put)
import Data.Bitraversable
import qualified Data.ByteString as B
import Data.List.NonEmpty as NE (nonEmpty, head, tail, reverse, (<|))
import Data.Text (singleton)
import Data.Text.Encoding
import System.Console.Haskeline

import Common
import Data
import Parse (isEmptyLine, parse, parseMany, errorBundlePretty)

type Error = String

type Environment = [(Symbol, (Object IORef))]

type EvalMonad = ExceptT Error (StateT EvalState IO)

data EvalState = EvalState
 { _globe :: Environment
 , _scope :: NonEmpty Environment
 , _locs :: [()]
 , _dyns :: Environment
 , _debug :: [String]
 }
$(makeLenses ''EvalState)

emptyState :: EvalState
emptyState = EvalState [] (pure []) [] [] []

class MonadRef m => MonadMutableRef m where
  modifyRef :: Ref m a -> (a -> a) -> m ()
  writeRef :: Ref m a -> a -> m ()

instance MonadMutableRef IO where
  modifyRef = modifyIORef
  writeRef = writeIORef

instance (MonadMutableRef m, MonadTrans t, Monad (t m)) => MonadMutableRef (t m) where
  modifyRef = lift .: modifyRef
  writeRef = lift .: writeRef

modifyRefM :: MonadMutableRef m => Ref m a -> (a -> m a) -> m ()
modifyRefM r f = readRef r >>= (f >=> writeRef r)

builtins :: EvalMonad ()
builtins = (globe <~) $ traverse ((\(s, o) -> (s,) <$> o) . first sym') $
  [ ("nil", sym "nil")
  , ("o", sym "o")
  , ("apply", sym "apply")
  , ("t", sym "t")
  , ("ins", sym "nil")
  , ("outs", sym "nil")
  , ("chars",
      let convert = flip B.foldl [] \acc -> (acc <>) . \w ->
              toObject . bool '0' '1' . testBit w <$> [0..7]
      in listToObject $ flip fmap [0..127] \i ->
           chr i .* (
            listToObject $ convert $ encodeUtf8 $ singleton $ chr i :: EvalMonad (Object IORef)
          )
    )
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
    ]
  where
    -- NOTE: sym and sym' are unsafe!
    --   They error if called on the empty string.
    --   Do not factor them out of this local scope.
    sym = pure . Symbol . sym'
    sym' :: String -> Symbol
    sym' = \case
      [] -> error "interpreter bug: called sym' with an empty string"
      x:xs -> MkSymbol (x :| xs)

envLookup :: Symbol -> EvalMonad (Object IORef)
envLookup s = do
  dyns' <- use dyns
  scope' <- use $ scope._Wrapped._1
  globe' <- use globe
  maybe
    (throwError . ("undefined symbol: " <>) =<< repr s)
    pure
    -- @incomplete: change behavior inside where
    (lookup s dyns' <|> lookup s scope' <|> lookup s globe')

function :: [Object IORef] -> MaybeT EvalMonad Closure
function x = case x of
  [Sym 'l' "it", Sym 'c' "lo", env, params, body] -> do
    env' <- properListOf env \case
      Pair r -> readRef r >>= \(MkPair tup) -> case tup of
        (Symbol var, val) -> pure (var, val)
        _ -> empty
      _ -> empty
    pure $ MkClosure env' params body
  _ -> empty

macro :: [Object IORef] -> MaybeT EvalMonad Closure
macro = \case
  [Sym 'l' "it", Sym 'm' "ac", m] -> MaybeT (properList m) >>= function
  _ -> empty

data Operator
  = Primitive Primitive
  | SpecialForm SpecialForm
  | Macro Closure
  | Closure Closure

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

specialForms :: [(String, SpecialForm)]
specialForms = (\f -> (formName f, f)) <$>
  [ Form1 "quote" pure
  , Lit
  , FormN "if" $ let
      go = \case
        [] -> pure $ Symbol Nil
        [x] -> evaluate x
        b:x:rest -> evaluate b >>= \case
          Symbol Nil -> go rest
          _ -> evaluate x
      in go
  , FormN "apply" $ let
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
              [] -> listToObject (pure (head ll) : fmap quote (tail ll)) >>= evaluate
                where ll = NE.reverse acc'
              c:cs -> go' (c<|acc') cs
      badParams = throwError "apply requires at least two parameters"
      in \case fa : r:est -> go (pure fa) (r:|est); _ -> badParams where
  , Form1 "where" let
      pushLoc = locs %= (():)
      popLoc = locs %= \case
        [] -> error "interpreter bug: should be impossible because of call to pushLoc"
        _:xs -> xs
      in \x -> pushLoc *> evaluate x <* popLoc
  , Form3 "dyn" \v x y -> case v of
        Symbol s -> evaluate x >>= \evX -> pushDyn evX *> evaluate y <* popDyn where
          pushDyn evX = dyns %= ((s,evX):)
          popDyn = dyns %= \case
            [] -> error "interpreter bug: should be impossible because of call to pushDyn"
            _:xs -> xs
        _ -> repr v >>= \rep -> throwError $ "dyn requires a symbol as its first argument. "
          <> rep <> " is not a symbol."
  , Form2 "after" \x y -> do
      preX <- get
      catchError (void $ evaluate x) $ const $ put preX
      evaluate y
  , Form2 "set" \var val -> case var of
      Symbol var' -> evaluate val >>= \val' -> (globe %= ((var', val'):)) $> val'
      _ -> throwError "set takes a symbol as its first argument"
  -- @incomplete: this is just an approximation, since I haven't learned
  -- yet what the full scope of def is.
  , Form3 "def" \n p e -> evaluate =<<
      ("set" ~~ n ~| ("lit" ~~ "clo" ~~ "nil" ~~ p ~| e))

  -- @incomplete: implement this in a way that cannot clash with user symbols
  , Form1 "~backquote" let
      go = \case
        Pair ref -> readRef ref >>= \(MkPair p) -> case p of
          (Sym '~' "comma", x) -> Left <$> evaluate x
          (Sym '~' "splice", x) -> Right <$> evaluate x
          _ -> bimapM go go p >>= \(x, either id id -> cdr) -> Left <$> case x of
            Left car -> car .* cdr
            Right prefix -> append prefix cdr
        x -> pure $ Left x
      in fmap (either id id) . go

  , Form3 "mac" \n p e -> evaluate =<<
      ("set" ~~ n ~| ("lit" ~~ "mac" ~| ("lit" ~~ "clo" ~~ "nil" ~~ p ~| e)))
  ]

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
      (Stream, Stream) -> False -- @incomplete: what should this be?
      (Pair ra, Pair rb) -> ra == rb
      _ -> False
  , Prim2 "join" (.*)
  , carAndCdr "car" fst 'a'
  , carAndCdr "cdr" snd 'd'
  , Prim1 "type" $ let
      go = \case
        Symbol _ -> 's' :| "ymbol"
        Character _ -> 'c' :| "har"
        Pair _ -> 'p' :| "air"
        Stream -> 's' :| "tream"
        WhereResult x -> go x
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
  ] where
      carAndCdr nm fn w = Prim1 nm \case
        Symbol Nil -> pure $ Symbol Nil
        -- If we are inside a "where", return the tuple and our location.
        -- Otherwise, we return the normal value.
        Pair ra -> readRef ra >>= \(MkPair tup) -> use locs >>= \case
          [] -> pure $ fn tup
          _ -> Pair ra ~| Sym @IORef w ""
        o -> repr o >>= \s -> throwError $ nm
          <> " is only defined on pairs and nil. " <> s <> " is neither of those."
      xarAndXdr nm which = Prim2 nm $ curry \case
        (Pair r, y) -> (modifyRef r $ MkPair . (which $ const y) . unPair) $> y
        (o, _) -> repr o >>= \s -> throwError $ nm
          <> " is only defined when the first argument is a pair. "
          <> s <> " is not a pair."

operator :: Object IORef -> EvalMonad (Maybe Operator)
operator = \case
  (specialForm -> Just f) -> pure $ Just $ SpecialForm f
  x -> evaluate x >>= \x' -> properList x' >>= \case
    (Just (primitive -> Just f)) -> pure $ Just $ Primitive f
    (Just l) -> runMaybeT $ (Closure <$> function l) <|> (Macro <$> macro l)
    Nothing -> pure Nothing

destructure :: Object IORef -> Object IORef -> EvalMonad Environment
destructure = go where
  go paramTree arg = case paramTree of
    -- @incomplete: Show more information about the function
    Character _ -> throwError $ "Invalid function definition. The parameter definition must "
      <> "consist entirely of symbols, but this one contained a character."
    Stream -> throwError $ "Invalid function definition. The parameter definition must "
      <> "consist entirely of symbols, but this one contained a stream."
    Symbol Nil -> case arg of
      Symbol Nil -> pure []
      _ -> throwError "Too many arguments in function call"
    Symbol s -> pure [(s, arg)]
    -- @incomplete: It seems dodgy to have a WhereResult here. Can we make that impossible?
    WhereResult x -> go x arg
    Pair pRef -> case arg of
      Pair aRef -> do
        MkPair (p1, p2) <- readRef pRef
        MkPair (a1, a2) <- readRef aRef
        -- @incomplete warn or error on duplicate symbol in params
        b1 <- go p1 a1
        b2 <- go p2 a2
        pure $ b1 <> b2
      _ -> throwError "Too few arguments in function call"

with :: MonadState s m => ASetter s s [e] [e] -> m a -> e -> m a
with l m e = push *> m <* pop where
  push = l %= (e:)
  pop = l %= \case
    [] -> error "interpreter bug: failed to pop stack frame"
    _:xs -> xs

evaluate :: Object IORef -> EvalMonad (Object IORef)
evaluate expr = bind (repr expr) $ with debug $ case expr of
  -- characters
  c@(Character _) -> pure c

  -- streams
  s@Stream -> pure s

  -- symbols
  (Symbol s@(MkSymbol (toList -> s'))) -> case s' of
    "globe" -> getEnv globe
    "scope" -> getEnv $ scope._Wrapped._1
    _ -> envLookup s
    where getEnv = use >=> listToObject . (fmap (uncurry (.*)))

  -- pairs
  Pair ref -> readRef ref >>= \(MkPair (_, argTree)) ->
    liftA2 (,) (string expr) (properList1 ref) >>= \case

      -- strings
      (Just _, _) -> pure expr

      -- operators with arguments
      (_, Just (op :| args)) -> operator op >>= maybe giveUp \case
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
        Closure (MkClosure env params body) -> do
          bound <- (traverse evaluate >=> listToObject . fmap pure >=> destructure params) args
          flip (with debug) "applying closure" $
            withScope (bound <> env) (evaluate body)
        Macro (MkClosure env params body) -> do
          bound <- destructure params argTree
          flip (with debug) "applying macro" $
            withScope (bound <> env) (evaluate body >>= evaluate)

      (Nothing, Nothing) -> giveUp

  WhereResult _ -> giveUp

  where
    withScope :: Environment -> EvalMonad a -> EvalMonad a
    withScope env a = push *> a <* pop where
      push = scope %= (env <|)
      pop = scope %= \case
        _:|[] -> error "interpreter bug: can't pop scope"
        _:|x:xs -> x:|xs
    giveUp = repr expr >>= \rep ->
      throwError $ "I don't know how to evaluate this yet: " <> rep

excessPrimParams :: String -> [a] -> Int -> EvalMonad b
excessPrimParams nm args n =
  throwError $ "Too many parameters in call to primitive " <> nm
    <> ". Got " <> show (length args) <> ", want at most " <> show n <> "."

wrongParamCount :: String -> [a] -> Int -> EvalMonad b
wrongParamCount nm args n =
  throwError $ "Wrong number of parameters in special form " <> nm
    <> ". Got " <> show (length args) <> ", want exactly " <> show n <> "."

fromBool :: Bool -> Object r
fromBool = \case
  True -> Sym 't' ""
  False -> Symbol Nil

runEval :: EvalMonad a -> EvalState -> IO (Either Error a, EvalState)
runEval = runStateT . runExceptT

readThenEval :: FilePath -> String -> EvalMonad (Object IORef)
readThenEval path =
  (>>= evaluate) .
  either (except . Left . errorBundlePretty) id .
  parse path

readThenRunEval :: FilePath -> String -> EvalState -> IO (Either Error (Object IORef), EvalState)
readThenRunEval p c s = flip runEval s $ readThenEval p c

builtinsIO :: IO EvalState
builtinsIO = snd <$> runEval builtins emptyState

bel :: FilePath -> IO (Either Error EvalState)
bel f = builtinsIO >>= \b -> readFile f >>= \s0 -> do
  prog <- either (die . errorBundlePretty) id (parseMany f s0)
  (x, s) <- runEval (traverse_ evaluate prog) b
  pure (x $> s)

repl :: IO ()
repl = do
  args <- getArgs
  s <- case args of
    [] -> builtinsIO
    [f] -> bel f >>= \es -> either die pure es
    _ -> die "Sorry, I can only handle up to one file"
  runInputT defaultSettings $ go s where
  go :: EvalState -> InputT IO ()
  go s = getInputLine "> " >>= \case
    Nothing -> pure ()
    Just line -> if isEmptyLine line then go s else do
      (x, s') <- lift $ readThenRunEval "repl" line s
      outputStrLn =<< either pure repr x
      -- loop, resetting state if there was an error
      go $ either (const s) (const s') x
