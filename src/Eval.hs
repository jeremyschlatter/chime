{-# LANGUAGE UndecidableInstances #-}
module Eval where

import BasePrelude hiding (evaluate, getEnv, head, tail)
import Control.Lens.Combinators
import Control.Lens.Operators hiding ((<|))
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import Data.List.NonEmpty as NE (nonEmpty, head, tail, reverse, (<|))
import Data.Text (singleton)
import Data.Text.Encoding
import Control.Monad.Trans.Class

import Common
import Data
import Parse (parse, errorBundlePretty)

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

instance MonadMutableRef IO where
  modifyRef = modifyIORef

instance (MonadMutableRef m, MonadTrans t, Monad (t m)) => MonadMutableRef (t m) where
  modifyRef = lift .: modifyRef

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
    (lookup s dyns' <|> lookup s scope' <|> lookup s globe')

pattern Sym       :: Char -> String -> Object s
pattern Sym n ame = Symbol (MkSymbol (n :| ame))

evaluate :: Object IORef -> EvalMonad (Object IORef)
evaluate = \case
  c@(Character _) -> pure c
  s@Stream -> pure s
  (Symbol s@(MkSymbol (toList -> s'))) -> case s' of
    "globe" -> getEnv globe
    "scope" -> getEnv scope
    _ -> envLookup s
    where
      getEnv =
        (>>= (listToPair . (fmap (fmap Pair . newRef . MkPair . first Symbol)))) .
         use
  x' -> properList x' >>= \case
    Just l -> case l of
      Symbol (MkSymbol f) : args ->
        let form1 = specialForm1 f args
            form3 = specialForm3 f args
        in case toList f of
          "quote" -> form1 pure
          "lit" -> pure x'
          "if" -> go args where
            go = \case
              [] -> pure $ Symbol Nil
              [x] -> evaluate x
              b:x:rest -> evaluate b >>= \case
                Symbol Nil -> go rest
                _ -> evaluate x
          "apply" -> case args of
            fa : r:est -> go (pure fa) (r:|est) where
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
                    quote x = listToPair [pure $ Sym 'q' "uote", pure x]
                    go' acc' = \case
                      [] -> listToPair (pure (head ll) : fmap quote (tail ll)) >>= evaluate
                        where ll = NE.reverse acc'
                      c:cs -> go' (c<|acc') cs
            _ -> throwE $ "apply requires at least two parameters"
          "where" -> pushLoc *> form1 evaluate <* popLoc where
            pushLoc = locs %= (():)
            popLoc = locs %= \case
              [] -> error "should be impossible because of call to pushLoc"
              _:xs -> xs
          "dyn" -> form3 $ \v x y -> case v of
            Symbol s -> evaluate x >>= \evX -> pushDyn evX *> evaluate y <* popDyn where
              pushDyn evX = dyns %= ((s,evX):)
              popDyn = dyns %= \case
                [] -> error "should be impossible because of call to pushDyn"
                _:xs -> xs
            _ -> repr v >>= \rep -> throwE $ "dyn requires a symbol as its first argument. "
              <> rep <> " is not a symbol."
          _ -> envLookup (MkSymbol f) >>= properList >>= \case
            Just [Sym 'l' "it", Sym 'p' "rim", Symbol (MkSymbol p1@(toList -> p))] ->
              case p of
                "id" -> prim2 $ pure . fromBool .: curry \case
                    (Symbol a, Symbol b) -> a == b
                    (Character a, Character b) -> a == b
                    (Stream, Stream) -> False -- @incomplete: what should this be?
                    (Pair ra, Pair rb) -> ra == rb
                    _ -> False
                "join" -> prim2 $ fmap Pair . newRef . MkPair .: (,)
                "car" -> carAndCdr fst 'a'
                "cdr" -> carAndCdr snd 'd'
                "type" -> prim1 $ pure . Symbol . MkSymbol . go where
                  go = \case
                    Symbol _ -> 's' :| "ymbol"
                    Character _ -> 'c' :| "har"
                    Pair _ -> 'p' :| "air"
                    Stream -> 's' :| "tream"
                    WhereResult x -> go x
                "xar" -> xarAndXdr first
                "xdr" -> xarAndXdr second
                "sym" -> prim1 $ \x -> string x >>= \s' -> case s' >>= nonEmpty of
                  Just s -> pure $ Symbol $ MkSymbol $ fmap unCharacter s
                  Nothing -> repr x >>= \rep -> throwE $ "sym is only defined on non-empty strings. "
                    <> rep <> " is not a non-empty string."
                "nom" -> prim1 $ \case
                  Sym n ame -> listToPair (pure . Character . MkCharacter <$> (n:ame))
                  x -> repr x >>= \rep -> throwE $ "nom is only defined on symbols. "
                    <> rep <> " is not a symbol."
                s -> throwE $ "no such primitive: " <> s
                where prim2 = primitive2 p1 args
                      prim1 = primitive1 p1 args
                      carAndCdr fn w = prim1 $ \case
                        Symbol Nil -> pure $ Symbol Nil
                        -- If we are inside a "where", return the tuple and our location.
                        -- Otherwise, we return the normal value.
                        Pair ra -> readRef ra >>= \(MkPair tup) -> use locs >>= \case
                          [] -> pure $ fn tup
                          _ -> listToPair $ pure <$> [Pair ra, Sym w ""]
                        o -> repr o >>= \s -> throwE $ toList p1
                          <> " is only defined on pairs and nil. " <> s <> " is neither of those."
                      xarAndXdr which = prim2 $ curry \case
                        (Pair r, y) -> (modifyRef r $ MkPair . (which $ const y) . unPair) $> y
                        (o, _) -> repr o >>= \s -> throwE $ toList p1
                          <> " is only defined when the first argument is a pair. "
                          <> s <> " is not a pair."
            _ -> throwE $ "I don't know how to evaluate this yet"
      _ -> string x' >>= \case
        Just _ -> pure x' -- x' is a string, and strings evaluate to themselves
        Nothing -> throwE $ "I don't know how to evaluate this yet"
    _ -> throwE $ "I don't know how to evaluate this yet"

excessPrimParams :: NonEmpty Char -> [a] -> Int -> EvalMonad b
excessPrimParams nm args n =
  throwE $ "Too many parameters in call to primitive " <> toList nm
    <> ". Got " <> show (length args) <> ", want at most " <> show n <> "."

wrongParamCount :: NonEmpty Char -> [a] -> Int -> EvalMonad b
wrongParamCount nm args n =
  throwE $ "Wrong number of parameters in special form " <> toList nm
    <> ". Got " <> show (length args) <> ", want exactly " <> show n <> "."

specialForm1
  :: NonEmpty Char
  -> [Object IORef]
  -> (Object IORef -> EvalMonad (Object IORef))
  -> EvalMonad (Object IORef)
specialForm1 nm args f = case args of
  [a] -> f a
  _ -> wrongParamCount nm args 1

specialForm3
  :: NonEmpty Char
  -> [Object IORef]
  -> (Object IORef -> Object IORef -> Object IORef -> EvalMonad (Object IORef))
  -> EvalMonad (Object IORef)
specialForm3 nm args f = case args of
  [a, b, c] -> f a b c
  _ -> wrongParamCount nm args 3

primitive1
  :: NonEmpty Char
  -> [Object IORef]
  -> (Object IORef -> EvalMonad (Object IORef))
  -> EvalMonad (Object IORef)
primitive1 nm args f = case args of
  [] -> call (Symbol Nil)
  [a] -> call a
  _ -> excessPrimParams nm args 1
  where
    call a = do
      a' <- evaluate a
      f a'

primitive2
  :: NonEmpty Char
  -> [Object IORef]
  -> (Object IORef -> Object IORef -> EvalMonad (Object IORef))
  -> EvalMonad (Object IORef)
primitive2 nm args f = case args of
  [] -> call (Symbol Nil) (Symbol Nil)
  [a] -> call a (Symbol Nil)
  [a, b] -> call a b
  _ -> excessPrimParams nm args 2
  where
    call a b = do
      a' <- evaluate a
      b' <- evaluate b
      f a' b'

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

repl :: IO ()
repl = builtinsIO >>= go where
  go s = do
    putStr "> "
    line <- getLine
    (x, s') <- readThenRunEval "repl" line s
    putStrLn =<< either pure repr x
    -- loop, resetting state if there was an error
    go $ either (const s) (const s') x
