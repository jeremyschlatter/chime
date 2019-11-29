module Eval where

import Control.Applicative
import Control.Lens.Combinators
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Bits
import Data.Bool
import qualified Data.ByteString as B
import Data.Char
import Data.List.NonEmpty
import Data.Text as T
import Data.Text.Encoding

import Data
import Parse (parse, errorBundlePretty)

type Error = String

type Environment = [(Symbol, Object)]

data EvalState = EvalState
 { _globe :: Environment
 , _scope :: Environment
 }
$(makeLenses ''EvalState)

builtins :: Environment
builtins = first sym' <$>
  [ ("nil", sym "nil")
  , ("o", sym "o")
  , ("apply", sym "apply")
  , ("t", sym "t")
  , ("ins", sym "nil")
  , ("outs", sym "nil")
  , ("chars",
      let convert = flip B.foldl [] \acc -> (acc <>) . \w ->
              Character . MkCharacter . bool '0' '1' . testBit w <$> [0..7]
      in listToPair $ flip fmap [0..127] \i ->
          Pair $ MkPair $ (,)
            (Character (MkCharacter (chr i)))
            (listToPair $ convert $ encodeUtf8 (T.singleton (chr i)))
    )
  ]
  where
    -- NOTE: sym and sym' are unsafe!
    --   They error if called on the empty string.
    --   Do not factor them out of this local scope.
    sym = Symbol . sym'
    sym' :: String -> Symbol
    sym' = \case
      [] -> error "developer error: called sym' with an empty string"
      x:xs -> MkSymbol (x :| xs)

newState :: EvalState
newState = EvalState builtins []

type EvalMonad = ExceptT Error (State EvalState)

readEval :: FilePath -> String -> EvalMonad Object
readEval path =
  (>>= evaluate) .
  except .
  first errorBundlePretty .
  parse path

envLookup :: Symbol -> Environment -> Maybe Object
envLookup = lookup

pattern Sym       :: Char -> String -> Object
pattern Sym n ame = Symbol (MkSymbol (n :| ame))

evaluate :: Object -> EvalMonad Object
evaluate = \case
  c@(Character _) -> pure c
  s@Stream -> pure s
  (Symbol s@(MkSymbol (toList -> s'))) -> case s' of
    "globe" -> getEnv globe
    "scope" -> getEnv scope
    _ -> do
      scope' <- use scope
      globe' <- use globe
      maybe (throwE $ "undefined symbol " <> s') pure
        (envLookup s scope' <|> envLookup s globe')
    where
      getEnv =
        (fmap (listToPair . (fmap (Pair . MkPair . first Symbol)))) .
        use
  (properList -> (Just l)) -> case l of
    [Sym 'q' "uote", a] -> pure a
    _ -> throwE $ "I don't know how to evaluate this yet"
  _ -> throwE $ "I don't know how to evaluate this yet"

runEval :: EvalMonad a -> EvalState -> (Either Error a, EvalState)
runEval = runState . runExceptT

readRunEval :: FilePath -> String -> EvalState -> (Either Error Object, EvalState)
readRunEval p c s = flip runEval s $ readEval p c

repl :: IO ()
repl = go newState where
  go s = do
    putStr "> "
    line <- getLine
    let (x, s') = readRunEval "repl" line s
    putStrLn $ either id repr x
    -- loop, resetting state if there was an error
    go $ either (const s) (const s') x
