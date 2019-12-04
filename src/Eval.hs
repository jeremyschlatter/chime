{-# LANGUAGE UndecidableInstances #-}
module Eval where

import BasePrelude hiding (evaluate, getEnv, head, tail)
import Control.Lens.Combinators hiding (op)
import Control.Lens.Operators hiding ((<|))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bitraversable
import qualified Data.ByteString as B
import Data.List.NonEmpty as NE (nonEmpty, head, tail, reverse, (<|))
import Data.Text (singleton)
import Data.Text.Encoding
import Control.Monad.Trans.Class

import Common
import Data
import Parse (isEmptyLine, parse, parseMany, errorBundlePretty)

type Error = String

type Environment = [(Symbol, (Object IORef))]

type EvalMonad = ExceptT Error (StateT EvalState IO)

data EvalState = EvalState
 { _globe :: Environment
 , _scope :: Environment
 , _locs :: [()]
 , _dyns :: Environment
 }
$(makeLenses ''EvalState)

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
              pure . Character . MkCharacter . bool '0' '1' . testBit w <$> [0..7]
      in listToPair $ flip fmap [0..127] \i -> do
           let car = Character $ MkCharacter $ chr i
           cdr <- listToPair $ convert $ encodeUtf8 $ singleton $ chr i
           Pair <$> (newRef $ MkPair (car, cdr))
    )
  ] <> fmap (\p -> (p, listToPair [sym "lit", sym "prim", sym p]))
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
      [] -> error "developer error: called sym' with an empty string"
      x:xs -> MkSymbol (x :| xs)

emptyState :: EvalState
emptyState = EvalState [] [] [] []

envLookup :: Symbol -> EvalMonad (Object IORef)
envLookup s = do
  dyns' <- use dyns
  scope' <- use scope
  globe' <- use globe
  maybe
    (throwE . ("undefined symbol: " <>) =<< repr s)
    pure
    -- @incomplete: change behavior inside where
    (lookup s dyns' <|> lookup s scope' <|> lookup s globe')

function :: [Object IORef] -> EvalMonad (Maybe (Environment, Object IORef, Object IORef))
function x = runMaybeT $ case x of
  [Sym 'l' "it", Sym 'c' "lo", env, params, body] -> do
    env' <- properListOf env \case
      Pair r -> readRef r >>= \(MkPair tup) -> case tup of
        (Symbol var, val) -> pure (var, val)
        _ -> empty
      _ -> empty
    pure (env', params, body)
  _ -> empty

bindVars :: [(Symbol, Object IORef)] -> Object IORef -> EvalMonad (Object IORef)
bindVars bindings = \case
  -- @incomplete: Do I need to change behavior here when inside where?
  s@(Symbol s') -> pure $ maybe s id $ lookup s' bindings
  Pair r -> readRef r >>= \(MkPair (car, cdr)) -> fmap Pair $ newRef =<< liftA2 (MkPair .: (,))
      (bindVars bindings car)
      (bindVars bindings cdr)
  x -> pure x

data Operator
  = Primitive Primitive
  | SpecialForm SpecialForm
  | Closure Environment (Object IORef) (Object IORef)

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
          Nothing -> throwE $
            "The last argument to apply must be a proper list, but was not. "
            <> "(apply should accept non-proper lists in some cases, but "
            <> "that has not been implemented yet)."
          Just lst -> go' acc lst where
            go' acc' = \case
              [] -> listToPair (pure (head ll) : fmap quote (tail ll)) >>= evaluate
                where ll = NE.reverse acc'
              c:cs -> go' (c<|acc') cs
      badParams = throwE "apply requires at least two parameters"
      in \case fa : r:est -> go (pure fa) (r:|est); _ -> badParams where
  , Form1 "where" let
      pushLoc = locs %= (():)
      popLoc = locs %= \case
        [] -> error "should be impossible because of call to pushLoc"
        _:xs -> xs
      in \x -> pushLoc *> evaluate x <* popLoc
  , Form3 "dyn" \v x y -> case v of
        Symbol s -> evaluate x >>= \evX -> pushDyn evX *> evaluate y <* popDyn where
          pushDyn evX = dyns %= ((s,evX):)
          popDyn = dyns %= \case
            [] -> error "should be impossible because of call to pushDyn"
            _:xs -> xs
        _ -> repr v >>= \rep -> throwE $ "dyn requires a symbol as its first argument. "
          <> rep <> " is not a symbol."
  , Form2 "after" \x y -> do
      preX <- lift get
      catchE (void $ evaluate x) $ const $ lift $ put preX
      evaluate y
  , Form2 "set" \var val -> case var of
      Symbol var' -> evaluate val >>= \val' -> (globe %= ((var', val'):)) $> val'
      _ -> throwE "set takes a symbol as its first argument"
  -- @incomplete: this is just an approximation, since I haven't learned
  -- yet what the full scope of def is.
  , Form3 "def" \n p e -> do
      -- @factoring: this should just say "evaluate (set n (lit clo nil p e))"
      -- see if I can make that more obvious
      f' <- listToPair (pure <$> [Sym 'l' "it", Sym 'c' "lo", Symbol Nil, p, e])
      evaluate =<< listToPair (pure <$> [Sym 's' "et", n, f'])

  -- @incomplete: implement this in a way that cannot clash with user symbols
  , Form1 "~backquote" let
      go = \case
        Pair ref -> readRef ref >>= \(MkPair p) -> case p of
          (Sym '~' "comma", x) -> Left <$> evaluate x
          (Sym '~' "splice", x) -> Right <$> evaluate x
          _ -> bimapM go go p >>= \(x, either id id -> cdr) -> Left <$> case x of
            Left car -> fmap Pair . newRef $ MkPair (car, cdr)
            Right prefix -> append prefix cdr
        x -> pure $ Left x
      in fmap (either id id) . go

  ]

append :: Object IORef -> Object IORef -> EvalMonad (Object IORef)
append a b = bimapM properList properList (a, b) >>= \case
  (Just a', Just b') -> pureListToPair $ a' <> b'
  _ -> throwE "non-lists in splice"

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
  , Prim2 "join" $ fmap Pair . newRef . MkPair .: (,)
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
      Nothing -> repr x >>= \rep -> throwE $ "sym is only defined on non-empty strings. "
        <> rep <> " is not a non-empty string."
  , Prim1 "nom" \case
      Sym n ame -> listToPair (pure . Character . MkCharacter <$> (n:ame))
      x -> repr x >>= \rep -> throwE $ "nom is only defined on symbols. "
        <> rep <> " is not a symbol."
  ] where
      carAndCdr nm fn w = Prim1 nm \case
        Symbol Nil -> pure $ Symbol Nil
        -- If we are inside a "where", return the tuple and our location.
        -- Otherwise, we return the normal value.
        Pair ra -> readRef ra >>= \(MkPair tup) -> use locs >>= \case
          [] -> pure $ fn tup
          _ -> listToPair $ pure <$> [Pair ra, Sym w ""]
        o -> repr o >>= \s -> throwE $ nm
          <> " is only defined on pairs and nil. " <> s <> " is neither of those."
      xarAndXdr nm which = Prim2 nm $ curry \case
        (Pair r, y) -> (modifyRef r $ MkPair . (which $ const y) . unPair) $> y
        (o, _) -> repr o >>= \s -> throwE $ nm
          <> " is only defined when the first argument is a pair. "
          <> s <> " is not a pair."

operator :: Object IORef -> EvalMonad (Maybe Operator)
operator = \case
  (specialForm -> Just f) -> pure $ Just $ SpecialForm f
  x -> evaluate x >>= \x' -> properList x' >>= \case
    (Just (primitive -> Just f)) -> pure $ Just $ Primitive f
    (Just l)  -> (\(e, p, b) -> Closure e p b) <$$> function l
    Nothing -> pure Nothing

destructure :: Object IORef -> Object IORef -> EvalMonad Environment
destructure = go where
  go paramTree arg = case paramTree of
    -- @incomplete: Show more information about the function
    Character _ -> throwE $ "Invalid function definition. The parameter definition must "
      <> "consist entirely of symbols, but this one contained a character."
    Stream -> throwE $ "Invalid function definition. The parameter definition must "
      <> "consist entirely of symbols, but this one contained a stream."
    Symbol Nil -> case arg of
      Symbol Nil -> pure []
      _ -> throwE "Too many arguments in function call"
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
      _ -> throwE "Too few arguments in function call"

evaluate :: Object IORef -> EvalMonad (Object IORef)
evaluate expr = case expr of
  -- characters
  c@(Character _) -> pure c

  -- streams
  s@Stream -> pure s

  -- symbols
  (Symbol s@(MkSymbol (toList -> s'))) -> case s' of
    "globe" -> getEnv globe
    "scope" -> getEnv scope
    _ -> envLookup s
    where
      getEnv = use >=>
        listToPair . (fmap (fmap Pair . newRef . MkPair . first Symbol))

  -- pairs
  Pair ref -> liftA2 (,) (string expr) (properList1 ref) >>= \case
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
      Closure env params body -> do
        bound <- (traverse evaluate >=> pureListToPair >=> destructure params) args
        boundAndQuoted <- traverse (\(s, x) -> quote x <&> (s,)) bound
        -- @perf this listToPair could be avoided if destructure operated on lists
        bindVars (boundAndQuoted <> env) body >>= evaluate

    (Nothing, Nothing) -> giveUp

  WhereResult _ -> giveUp

  where
    giveUp = repr expr >>= \rep ->
      throwE $ "I don't know how to evaluate this yet: " <> rep

excessPrimParams :: String -> [a] -> Int -> EvalMonad b
excessPrimParams nm args n =
  throwE $ "Too many parameters in call to primitive " <> nm
    <> ". Got " <> show (length args) <> ", want at most " <> show n <> "."

wrongParamCount :: String -> [a] -> Int -> EvalMonad b
wrongParamCount nm args n =
  throwE $ "Wrong number of parameters in special form " <> nm
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

readThenRunEval
  :: FilePath -> String -> EvalState -> IO (Either Error (Object IORef), EvalState)
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
  go s where
  go s = do
    putStr "> "
    line <- catchIOError getLine \e ->
      bool (ioError e) (putStrLn "" *> exitSuccess) (isEOFError e)
    if isEmptyLine line then go s else do
      (x, s') <- readThenRunEval "repl" line s
      putStrLn =<< either pure repr x
      -- loop, resetting state if there was an error
      go $ either (const s) (const s') x
