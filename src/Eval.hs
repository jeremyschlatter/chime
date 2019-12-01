module Eval where

import BasePrelude hiding (evaluate, getEnv)
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import Data.Text as T hiding (length)
import Data.Text.Encoding

import Common
import Data
import Parse (parse, errorBundlePretty)

type Error = String

type Environment = [(Symbol, (Object IORef))]

type EvalMonad = ExceptT Error (StateT EvalState IO)

data EvalState = EvalState
 { _globe :: Environment
 , _scope :: Environment
 }
$(makeLenses ''EvalState)

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
           cdr <- listToPair $ convert $ encodeUtf8 $ T.singleton $ chr i
           Pair <$> (newRef $ MkPair (car, cdr))
    )
  ] <> fmap (\p -> (p, listToPair [sym "lit", sym "prim", sym p]))
    [ "id"
    , "join"
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
emptyState = EvalState [] []

envLookup :: Symbol -> EvalMonad (Object IORef)
envLookup s = do
  scope' <- use scope
  globe' <- use globe
  maybe
    (throwE . ("undefined symbol: " <>) =<< repr s)
    pure
    (lookup s scope' <|> lookup s globe')

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
  x -> properList x >>= \case
    Just l -> case l of
      [Sym 'q' "uote", a] -> pure a
      Symbol f : args -> envLookup f >>= properList >>= \case
        Just [Sym 'l' "it", Sym 'p' "rim", Symbol (MkSymbol p1@(toList -> p))] ->
          case p of
            "id" -> prim2 $ pure . fromBool .: curry \case
                (Symbol a, Symbol b) -> a == b
                (Character a, Character b) -> a == b
                (Stream, Stream) -> False -- @incomplete: what should this be?
                (Pair ra, Pair rb) -> ra == rb
                _ -> False
            "join" -> prim2 $ fmap Pair . newRef . MkPair .: (,)
            s -> throwE $ "no such primitive: " <> s
            where prim2 = primitive2 p1 args
        _ -> throwE $ "I don't know how to evaluate this yet"
      _ -> throwE $ "I don't know how to evaluate this yet"
    _ -> throwE $ "I don't know how to evaluate this yet"

primitive2
  :: NonEmpty Char
  -> [Object IORef]
  -> (Object IORef -> Object IORef -> EvalMonad (Object IORef))
  -> EvalMonad (Object IORef)
primitive2 nm args f = case args of
  [] -> call (Symbol Nil) (Symbol Nil)
  [a] -> call a (Symbol Nil)
  [a, b] -> call a b
  _ -> throwE $ "Too many parameters in call to " <> toList nm
    <> ". Got " <> show (Prelude.length args) <> ", want at most 2."
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
